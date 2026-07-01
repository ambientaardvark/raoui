/* Linux sandbox for the run_r AI-eval path: Landlock for filesystem access plus
   a seccomp-bpf filter for network/exec, replacing the macOS Seatbelt profile.
   See sandbox_linux.h for the posture this mirrors. */

#define _GNU_SOURCE
#include "sandbox_linux.h"

#if defined(__linux__)

#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stddef.h>
#include <errno.h>
#include <limits.h>
#include <fcntl.h>
#include <unistd.h>
#include <dirent.h>
#include <sys/stat.h>
#include <sys/syscall.h>
#include <sys/socket.h>
#include <sys/prctl.h>

#include <linux/landlock.h>
#include <linux/seccomp.h>
#include <linux/filter.h>
#include <linux/audit.h>

/* The scratch directory AI code is allowed to write to (mirrors the macOS
   /private/tmp/raoui-sandbox subpath). */
#define RUNR_SCRATCH_DIR "/tmp/raoui-sandbox"

/* ---- Landlock syscall wrappers (no glibc stubs ship for these) ---- */

#ifndef __NR_landlock_create_ruleset
#define __NR_landlock_create_ruleset 444
#endif
#ifndef __NR_landlock_add_rule
#define __NR_landlock_add_rule 445
#endif
#ifndef __NR_landlock_restrict_self
#define __NR_landlock_restrict_self 446
#endif

static long ll_create_ruleset(const struct landlock_ruleset_attr *attr,
                              size_t size, uint32_t flags) {
    return syscall(__NR_landlock_create_ruleset, attr, size, flags);
}
static long ll_add_rule(int ruleset_fd, enum landlock_rule_type type,
                        const void *rule_attr, uint32_t flags) {
    return syscall(__NR_landlock_add_rule, ruleset_fd, type, rule_attr, flags);
}
static long ll_restrict_self(int ruleset_fd, uint32_t flags) {
    return syscall(__NR_landlock_restrict_self, ruleset_fd, flags);
}

/* Filesystem rights we govern. EXECUTE is deliberately omitted: leaving it
   unhandled keeps PROT_EXEC mmap (i.e. dlopen of R package .so files) working,
   while seccomp blocks real program execution. REFER/IOCTL_DEV are also left
   unhandled so ioctls on the output pipe aren't disturbed and the handled set
   stays within older ABIs. */
#define FS_READ_RIGHTS  (LANDLOCK_ACCESS_FS_READ_FILE | \
                         LANDLOCK_ACCESS_FS_READ_DIR)
#define FS_WRITE_RIGHTS (LANDLOCK_ACCESS_FS_WRITE_FILE | \
                         LANDLOCK_ACCESS_FS_TRUNCATE | \
                         LANDLOCK_ACCESS_FS_REMOVE_FILE | \
                         LANDLOCK_ACCESS_FS_REMOVE_DIR | \
                         LANDLOCK_ACCESS_FS_MAKE_REG | \
                         LANDLOCK_ACCESS_FS_MAKE_DIR | \
                         LANDLOCK_ACCESS_FS_MAKE_SYM | \
                         LANDLOCK_ACCESS_FS_MAKE_FIFO | \
                         LANDLOCK_ACCESS_FS_MAKE_SOCK | \
                         LANDLOCK_ACCESS_FS_MAKE_CHAR | \
                         LANDLOCK_ACCESS_FS_MAKE_BLOCK)
/* Rights that are valid on a regular (non-directory) file. Granting a
   directory-only right (e.g. READ_DIR) on a file makes landlock_add_rule
   reject the whole rule with EINVAL, so file grants are masked to this set. */
#define FS_FILE_RIGHTS  (LANDLOCK_ACCESS_FS_READ_FILE | \
                         LANDLOCK_ACCESS_FS_WRITE_FILE | \
                         LANDLOCK_ACCESS_FS_TRUNCATE | \
                         LANDLOCK_ACCESS_FS_EXECUTE)

/* System roots R and the dynamic loader touch lazily after the fork. None are
   ancestors of a normal $HOME (note: /var is intentionally excluded, since
   ostree distros put homes under /var/home and granting it would re-expose
   secrets; /proc and /sys are excluded so AI code can't read this process's
   environment via /proc/self/environ). */
static const char *const SYS_READ_ROOTS[] = {
    "/usr", "/lib", "/lib64", "/bin", "/sbin", "/etc", "/dev", "/tmp", NULL,
};

/* Credential stores under $HOME that stay unreadable even if cwd == $HOME. */
static const char *const HOME_SECRETS[] = {
    ".ssh", ".aws", ".gnupg", ".config", ".netrc", ".Renviron", ".Rhistory",
    ".bash_history", ".zsh_history", ".docker", ".kube", NULL,
};

/* Grant `access` on `path` (a directory or file). A path that can't be opened
   (e.g. does not exist) is silently skipped — there is nothing to grant. */
static int grant_path(int ruleset_fd, const char *path, uint64_t access) {
    int fd = open(path, O_PATH | O_CLOEXEC);
    if (fd < 0)
        return 0;
    /* Directory-only rights are invalid on a regular file and would make the
       rule fail with EINVAL, so drop them for non-directories. */
    struct stat st;
    if (fstat(fd, &st) == 0 && !S_ISDIR(st.st_mode))
        access &= FS_FILE_RIGHTS;
    if (access == 0) { /* nothing left to grant */
        close(fd);
        return 0;
    }
    struct landlock_path_beneath_attr pb = {
        .allowed_access = access,
        .parent_fd = fd,
    };
    long rc = ll_add_rule(ruleset_fd, LANDLOCK_RULE_PATH_BENEATH, &pb, 0);
    int saved = errno;
    close(fd);
    if (rc != 0) {
        errno = saved;
        return -1;
    }
    return 0;
}

static int is_secret_name(const char *name) {
    for (size_t i = 0; HOME_SECRETS[i]; i++)
        if (strcmp(name, HOME_SECRETS[i]) == 0)
            return 1;
    return 0;
}

/* Grant read on every immediate child of $HOME except the credential stores,
   so AI code can read data files in $HOME without reaching ~/.ssh etc. */
static void grant_home_minus_secrets(int ruleset_fd, const char *home,
                                     uint64_t read_access) {
    DIR *d = opendir(home);
    if (!d)
        return;
    struct dirent *ent;
    while ((ent = readdir(d)) != NULL) {
        const char *n = ent->d_name;
        if (strcmp(n, ".") == 0 || strcmp(n, "..") == 0 || is_secret_name(n))
            continue;
        char child[PATH_MAX];
        if ((size_t)snprintf(child, sizeof child, "%s/%s", home, n) >= sizeof child)
            continue;
        grant_path(ruleset_fd, child, read_access);
    }
    closedir(d);
}

/* Resolve both and report whether `a` and `b` name the same path. */
static int same_path(const char *a, const char *b) {
    char ra[PATH_MAX], rb[PATH_MAX];
    if (!realpath(a, ra) || !realpath(b, rb))
        return 0;
    return strcmp(ra, rb) == 0;
}

/* Report whether `anc` is a strict ancestor directory of `desc` (so granting
   `anc` wholesale would expose `desc`). Operates on resolved paths. */
static int is_strict_ancestor(const char *anc, const char *desc) {
    char ra[PATH_MAX], rd[PATH_MAX];
    if (!realpath(anc, ra) || !realpath(desc, rd))
        return 0;
    size_t la = strlen(ra);
    if (la == 0 || strncmp(ra, rd, la) != 0)
        return 0;
    /* "/" is an ancestor of everything; otherwise require a "/" boundary. */
    return strcmp(ra, "/") == 0 || rd[la] == '/';
}

/* Grant read on a tree while keeping $HOME credential stores unreadable:
   - path == $HOME            -> expand to children minus secrets
   - path is an ancestor of $HOME -> skip (granting it would expose secrets)
   - otherwise                -> grant directly */
static int grant_read_tree(int ruleset_fd, const char *path, const char *home,
                           uint64_t read_access) {
    if (home && *home) {
        if (same_path(path, home)) {
            grant_home_minus_secrets(ruleset_fd, home, read_access);
            return 0;
        }
        if (is_strict_ancestor(path, home))
            return 0;
    }
    return grant_path(ruleset_fd, path, read_access);
}

/* Apply the Landlock filesystem allowlist. Returns 0 on success. */
static int apply_landlock(const char *dyn_read_paths, const char *home,
                          const char **err) {
    long abi = ll_create_ruleset(NULL, 0, LANDLOCK_CREATE_RULESET_VERSION);
    if (abi < 1) {
        if (err) *err = "Landlock is unavailable on this kernel";
        return -1;
    }

    uint64_t handled = FS_READ_RIGHTS | FS_WRITE_RIGHTS;
    if (abi < 3)
        handled &= ~LANDLOCK_ACCESS_FS_TRUNCATE; /* TRUNCATE arrived in ABI 3 */

    const uint64_t read_access = FS_READ_RIGHTS & handled;
    const uint64_t write_access = (FS_READ_RIGHTS | FS_WRITE_RIGHTS) & handled;

    struct landlock_ruleset_attr rattr = {
        .handled_access_fs = handled,
        .handled_access_net = 0,
    };
    int ruleset_fd = (int)ll_create_ruleset(&rattr, sizeof rattr, 0);
    if (ruleset_fd < 0) {
        if (err) *err = "landlock_create_ruleset failed";
        return -1;
    }

    /* Reads: system roots, then the dynamic R paths (R.home/.libPaths/tempdir/
       cwd), each routed through grant_read_tree so $HOME secrets stay closed. */
    for (size_t i = 0; SYS_READ_ROOTS[i]; i++)
        grant_read_tree(ruleset_fd, SYS_READ_ROOTS[i], home, read_access);

    if (dyn_read_paths) {
        char *dup = strdup(dyn_read_paths);
        if (dup) {
            char *save = NULL;
            for (char *tok = strtok_r(dup, "\n", &save); tok;
                 tok = strtok_r(NULL, "\n", &save))
                if (*tok)
                    grant_read_tree(ruleset_fd, tok, home, read_access);
            free(dup);
        }
    }

    /* Writes: the scratch dir (created so the rule has an inode) and /dev/null. */
    mkdir(RUNR_SCRATCH_DIR, 0700); /* ok if it already exists */
    grant_path(ruleset_fd, RUNR_SCRATCH_DIR, write_access);
    grant_path(ruleset_fd, "/dev/null", LANDLOCK_ACCESS_FS_WRITE_FILE & handled);

    if (prctl(PR_SET_NO_NEW_PRIVS, 1, 0, 0, 0) != 0) {
        if (err) *err = "prctl(PR_SET_NO_NEW_PRIVS) failed";
        close(ruleset_fd);
        return -1;
    }
    if (ll_restrict_self(ruleset_fd, 0) != 0) {
        if (err) *err = "landlock_restrict_self failed";
        close(ruleset_fd);
        return -1;
    }
    close(ruleset_fd);
    return 0;
}

/* ---- seccomp: deny network socket creation and program execution ---- */

#ifndef SECCOMP_RET_KILL_PROCESS
#define SECCOMP_RET_KILL_PROCESS 0x80000000U
#endif

#define DENY_EACCES (SECCOMP_RET_ERRNO | (EACCES & SECCOMP_RET_DATA))

static int apply_seccomp(const char **err) {
    struct sock_filter filter[] = {
        /* Reject calls from an unexpected architecture outright. */
        BPF_STMT(BPF_LD | BPF_W | BPF_ABS, offsetof(struct seccomp_data, arch)),
        BPF_JUMP(BPF_JMP | BPF_JEQ | BPF_K, AUDIT_ARCH_X86_64, 1, 0),
        BPF_STMT(BPF_RET | BPF_K, SECCOMP_RET_KILL_PROCESS),

        /* Load the syscall number. */
        BPF_STMT(BPF_LD | BPF_W | BPF_ABS, offsetof(struct seccomp_data, nr)),

        /* Deny program execution (process-exec*). */
        BPF_JUMP(BPF_JMP | BPF_JEQ | BPF_K, __NR_execve, 0, 1),
        BPF_STMT(BPF_RET | BPF_K, DENY_EACCES),
        BPF_JUMP(BPF_JMP | BPF_JEQ | BPF_K, __NR_execveat, 0, 1),
        BPF_STMT(BPF_RET | BPF_K, DENY_EACCES),

        /* Deny network (network*): allow only AF_UNIX sockets. */
        BPF_JUMP(BPF_JMP | BPF_JEQ | BPF_K, __NR_socket, 0, 3),
        BPF_STMT(BPF_LD | BPF_W | BPF_ABS, offsetof(struct seccomp_data, args[0])),
        BPF_JUMP(BPF_JMP | BPF_JEQ | BPF_K, AF_UNIX, 1, 0),
        BPF_STMT(BPF_RET | BPF_K, DENY_EACCES),

        /* Everything else is allowed. */
        BPF_STMT(BPF_RET | BPF_K, SECCOMP_RET_ALLOW),
    };
    struct sock_fprog prog = {
        .len = (unsigned short)(sizeof filter / sizeof filter[0]),
        .filter = filter,
    };
    /* PR_SET_NO_NEW_PRIVS was already set by apply_landlock. */
    if (prctl(PR_SET_SECCOMP, SECCOMP_MODE_FILTER, &prog, 0, 0) != 0) {
        if (err) *err = "prctl(PR_SET_SECCOMP) failed";
        return -1;
    }
    return 0;
}

int raoui_sandbox_apply(const char *dyn_read_paths, const char *home,
                        const char **err) {
    if (err) *err = NULL;
    if (apply_landlock(dyn_read_paths, home, err) != 0)
        return -1;
    if (apply_seccomp(err) != 0)
        return -1;
    return 0;
}

#else /* !__linux__ */

int raoui_sandbox_apply(const char *dyn_read_paths, const char *home,
                        const char **err) {
    (void)dyn_read_paths;
    (void)home;
    if (err) *err = "Linux sandbox invoked on a non-Linux platform";
    return -1;
}

#endif /* __linux__ */
