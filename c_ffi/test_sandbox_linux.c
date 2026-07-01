/* Standalone enforcement test for the Linux run_r sandbox.
   Build (from c_ffi/):
     cc -std=c99 -D_GNU_SOURCE -Wall -o test_sandbox_linux \
        test_sandbox_linux.c sandbox_linux.c
   Run: ./test_sandbox_linux  (exit 0 = all checks passed)

   Applies raoui_sandbox_apply() once to this process (simulating cwd == $HOME),
   then exercises the filesystem / network / exec rules. A scratch $HOME is
   created under the real $HOME so it is NOT covered by the system read roots. */

#define _GNU_SOURCE
#include "sandbox_linux.h"

#include <dlfcn.h>
#include <errno.h>
#include <fcntl.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <sys/socket.h>
#include <sys/stat.h>

static int failures = 0;

#define CHECK(cond, msg) do { \
    if (cond) { printf("  PASS: %s\n", (msg)); } \
    else { printf("  FAIL: %s\n", (msg)); failures++; } \
} while (0)

static int can_read(const char *path) {
    int fd = open(path, O_RDONLY | O_CLOEXEC);
    if (fd >= 0) { close(fd); return 1; }
    return 0;
}

/* Try to create+open a file for writing; clean it up on success. */
static int can_write(const char *path) {
    int fd = open(path, O_WRONLY | O_CREAT | O_CLOEXEC, 0600);
    if (fd >= 0) { close(fd); unlink(path); return 1; }
    return 0;
}

static void write_file(const char *path, const char *content) {
    int fd = open(path, O_WRONLY | O_CREAT | O_TRUNC, 0600);
    if (fd >= 0) { (void)!write(fd, content, strlen(content)); close(fd); }
}

int main(void) {
    const char *real_home = getenv("HOME");
    if (!real_home || !*real_home) {
        fprintf(stderr, "HOME not set; cannot run test\n");
        return 2;
    }

    /* A throwaway $HOME under the real home (so system read roots don't cover
       it), populated with a data file and a fake credential store. */
    char fake_home[1024];
    snprintf(fake_home, sizeof fake_home, "%s/.raoui_sbtest_XXXXXX", real_home);
    if (!mkdtemp(fake_home)) { perror("mkdtemp"); return 2; }

    char data_file[1100], ssh_dir[1100], ssh_key[1200], new_file[1100];
    snprintf(data_file, sizeof data_file, "%s/data.csv", fake_home);
    snprintf(ssh_dir, sizeof ssh_dir, "%s/.ssh", fake_home);
    snprintf(ssh_key, sizeof ssh_key, "%s/id_rsa", ssh_dir);
    snprintf(new_file, sizeof new_file, "%s/should_not_write", fake_home);
    mkdir(ssh_dir, 0700);
    write_file(data_file, "x,y\n1,2\n");
    write_file(ssh_key, "TOP SECRET KEY\n");

    /* Pre-sandbox sanity: everything is readable/writable now. */
    CHECK(can_read(data_file), "pre-sandbox: data file readable");
    CHECK(can_read(ssh_key), "pre-sandbox: secret readable");

    /* Apply the sandbox as if cwd == fake_home. */
    const char *err = NULL;
    int rc = raoui_sandbox_apply(fake_home, fake_home, &err);
    if (rc != 0) {
        fprintf(stderr, "raoui_sandbox_apply failed: %s\n", err ? err : "?");
        return 2;
    }
    printf("sandbox applied; running checks\n");

    /* --- Filesystem reads --- */
    CHECK(can_read("/etc/hostname") || can_read("/etc/passwd"),
          "read allowed: system root /etc");
    CHECK(can_read(data_file), "read allowed: non-secret file in $HOME (cwd)");
    CHECK(!can_read(ssh_key), "read denied: ~/.ssh credential file");
    CHECK(!can_read(ssh_dir), "read denied: ~/.ssh directory");

    /* --- Filesystem writes --- */
    CHECK(can_write("/tmp/raoui-sandbox/probe"), "write allowed: scratch dir");
    CHECK(can_write("/dev/null"), "write allowed: /dev/null");
    CHECK(!can_write(new_file), "write denied: new file in $HOME");
    CHECK(!can_write("/tmp/raoui_outside_probe"),
          "write denied: /tmp outside scratch");

    /* --- Network --- */
    errno = 0;
    int s_inet = socket(AF_INET, SOCK_STREAM, 0);
    CHECK(s_inet < 0 && errno == EACCES, "network denied: AF_INET socket -> EACCES");
    if (s_inet >= 0) close(s_inet);
    errno = 0;
    int s_inet6 = socket(AF_INET6, SOCK_STREAM, 0);
    CHECK(s_inet6 < 0 && errno == EACCES, "network denied: AF_INET6 socket -> EACCES");
    if (s_inet6 >= 0) close(s_inet6);
    int s_unix = socket(AF_UNIX, SOCK_STREAM, 0);
    CHECK(s_unix >= 0, "AF_UNIX socket still allowed");
    if (s_unix >= 0) close(s_unix);

    /* --- Exec --- */
    char *const argv[] = { (char *)"/bin/true", NULL };
    errno = 0;
    int ex = execve("/bin/true", argv, environ);
    CHECK(ex < 0 && errno == EACCES, "exec denied: execve -> EACCES");

    /* --- dlopen still works (R loads package .so files this way) --- */
    void *h = dlopen("libz.so.1", RTLD_NOW | RTLD_LOCAL);
    CHECK(h != NULL, "dlopen of a system shared library still works");
    if (h) dlclose(h);

    /* Best-effort cleanup (writes to $HOME are now denied, so this no-ops
       under the sandbox; the dir is tiny and lives under the user's home). */
    unlink(ssh_key); rmdir(ssh_dir); unlink(data_file); rmdir(fake_home);

    printf("\n%s (%d failure%s)\n",
           failures == 0 ? "ALL CHECKS PASSED" : "SOME CHECKS FAILED",
           failures, failures == 1 ? "" : "s");
    return failures == 0 ? 0 : 1;
}
