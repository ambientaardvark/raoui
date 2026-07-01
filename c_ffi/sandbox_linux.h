#ifndef SANDBOX_LINUX_H
#define SANDBOX_LINUX_H

/* Confine the current process for a run_r evaluation (Linux only).
 *
 * Must be called post-fork in the single-threaded child, before any AI-supplied
 * R code runs. It mirrors the macOS Seatbelt posture used elsewhere:
 *   - filesystem: a Landlock allowlist. Reads are permitted on system roots and
 *     the R machinery / .libPaths() / tempdir() / cwd carried in
 *     `dyn_read_paths`; writes are confined to a scratch dir and /dev/null.
 *     Credential stores under $HOME (~/.ssh, ~/.aws, ...) stay unreadable even
 *     when cwd == $HOME, because that path is expanded to its children minus the
 *     secret entries (Landlock rules are additive, so a deny is expressed by not
 *     granting). Directories that *contain* $HOME are not granted wholesale.
 *   - network + exec: a seccomp-bpf filter that denies creating non-AF_UNIX
 *     sockets and denies execve/execveat, each with a clean EACCES.
 *
 * dyn_read_paths: newline-separated absolute paths from the live R session
 *   (R.home(), .libPaths(), tempdir(), getwd()); may be NULL.
 * home: value of $HOME (may be NULL).
 * err: on failure, set to a static human-readable reason (do not free); may be
 *   NULL.
 *
 * Returns 0 on success. Returns non-zero if the kernel lacks Landlock or any
 * step fails: callers MUST treat that as fatal and refuse to run the code (fail
 * closed) rather than proceed unconfined. */
int raoui_sandbox_apply(const char *dyn_read_paths, const char *home,
                        const char **err);

#endif /* SANDBOX_LINUX_H */
