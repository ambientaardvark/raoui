/* Seatbelt spike (throwaway): validate that a macOS sandbox profile blocks the
 * effects run_r must forbid (file writes, network, exec) while allowing pure
 * computation and reads. Compiled standalone, NOT via dune:
 *
 *   cc -o /tmp/seatbelt_test c_ffi/seatbelt_test.c -Wno-deprecated-declarations
 *
 * Runs ONE operation per process: sandbox_init() is irreversible and
 * process-wide, and some violations SIGKILL rather than return, so isolating
 * each op in its own process keeps a hard kill from aborting the others. The
 * driver (seatbelt_test.sh) runs every op and tabulates PASS/FAIL. */

#include <arpa/inet.h>
#include <errno.h>
#include <fcntl.h>
#include <netinet/in.h>
#include <sandbox.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/socket.h>
#include <sys/wait.h>
#include <unistd.h>

/* Real /tmp path (macOS /tmp is a symlink to /private/tmp; seatbelt matches on
 * the resolved path). The one directory the sandbox is allowed to write to. */
#define SCRATCH "/private/tmp/raoui-sandbox"

/* Sandbox Profile Language (SBPL), "allow default, deny the dangerous classes"
 * posture: start from a working process and subtract effects, so an
 * already-initialized R won't crash on a missing allow. Last matching rule
 * wins, so the scratch/dev-null allow re-permits writes after the blanket
 * file-write* deny. */
static const char *PROFILE =
    "(version 1)\n"
    "(allow default)\n"
    "(deny file-write*)\n"
    "(allow file-write*\n"
    "  (subpath \"" SCRATCH "\")\n"
    "  (literal \"/dev/null\"))\n"
    "(deny network*)\n"
    "(deny process-exec*)\n";

/* Apply the sandbox; abort loudly (fail early) if the profile is malformed. */
static void enter_sandbox(void) {
  char *err = NULL;
#pragma clang diagnostic push
#pragma clang diagnostic ignored "-Wdeprecated-declarations"
  int rc = sandbox_init(PROFILE, 0, &err);
#pragma clang diagnostic pop
  if (rc != 0) {
    fprintf(stderr, "sandbox_init failed: %s\n", err ? err : "(no message)");
    sandbox_free_error(err);
    exit(2);
  }
}

/* Report helpers: one line, "result=OK" (action completed) or
 * "result=BLOCKED" (action refused), with the syscall + errno on a block. */
static void ok(const char *op, const char *detail) {
  printf("op=%s result=OK detail=%s\n", op, detail);
}
static void blocked(const char *op, const char *call) {
  printf("op=%s result=BLOCKED detail=%s: %s (errno=%d)\n", op, call,
         strerror(errno), errno);
}

static int op_compute(void) {
  /* Pure in-memory work: must always succeed. */
  volatile long sum = 0;
  for (long i = 0; i < 10000000L; i++) sum += i;
  char buf[64];
  snprintf(buf, sizeof buf, "sum=%ld", (long)sum);
  ok("compute", buf);
  return 0;
}

static int op_read(void) {
  /* R needs to read shared libs, locale data, etc. — reads stay allowed. */
  int fd = open("/etc/hosts", O_RDONLY);
  if (fd < 0) {
    blocked("read", "open");
    return 0;
  }
  char b;
  read(fd, &b, 1);
  close(fd);
  ok("read", "/etc/hosts");
  return 0;
}

/* Try to create + write a file at [path]; report OK or BLOCKED. */
static int try_write(const char *op, const char *path) {
  int fd = open(path, O_WRONLY | O_CREAT | O_TRUNC, 0644);
  if (fd < 0) {
    blocked(op, "open");
    return 0;
  }
  if (write(fd, "x", 1) < 0) {
    blocked(op, "write");
    close(fd);
    return 0;
  }
  close(fd);
  ok(op, path);
  return 0;
}

static int op_write_home(void) {
  const char *home = getenv("HOME");
  char path[1024];
  snprintf(path, sizeof path, "%s/raoui_seatbelt_probe.txt",
           home ? home : "/tmp");
  return try_write("write_home", path);
}

static int op_write_scratch(void) {
  return try_write("write_scratch", SCRATCH "/probe.txt");
}

static int op_net(void) {
  /* Connect to a (closed) loopback port: if the network stack is reachable we
   * get ECONNREFUSED fast (NOT blocked); seatbelt denial shows as EPERM. No
   * internet dependency, no hang. */
  int fd = socket(AF_INET, SOCK_STREAM, 0);
  if (fd < 0) {
    blocked("net", "socket");
    return 0;
  }
  struct sockaddr_in addr;
  memset(&addr, 0, sizeof addr);
  addr.sin_family = AF_INET;
  addr.sin_port = htons(9); /* discard port, almost certainly closed */
  inet_pton(AF_INET, "127.0.0.1", &addr.sin_addr);
  int rc = connect(fd, (struct sockaddr *)&addr, sizeof addr);
  if (rc < 0 && errno == EPERM) {
    blocked("net", "connect");
    close(fd);
    return 0;
  }
  /* rc==0 or ECONNREFUSED both mean the syscall ran: network not blocked. */
  char buf[64];
  snprintf(buf, sizeof buf, "connect rc=%d errno=%d (%s)", rc, errno,
           rc < 0 ? strerror(errno) : "connected");
  ok("net", buf);
  close(fd);
  return 0;
}

static int op_exec(void) {
  /* Mirrors R's system(): fork+exec. process-exec* denial should stop the
   * child from exec'ing, so system() returns non-zero. */
  int rc = system("/bin/echo seatbelt-exec-ran >/dev/null 2>&1");
  if (rc != 0) {
    /* Not an errno-based block: the parent's system() returns fine, but the
     * forked child can't exec, so it exits 127. Report that directly. */
    printf("op=exec result=BLOCKED detail=system() rc=%d, child exit=%d "
           "(exec refused)\n",
           rc, WIFEXITED(rc) ? WEXITSTATUS(rc) : -1);
    return 0;
  }
  ok("exec", "system() ran /bin/echo");
  return 0;
}

int main(int argc, char **argv) {
  if (argc != 2) {
    fprintf(stderr, "usage: %s <compute|read|write_home|write_scratch|net|exec>\n",
            argv[0]);
    return 3;
  }
  enter_sandbox();
  const char *op = argv[1];
  if (strcmp(op, "compute") == 0) return op_compute();
  if (strcmp(op, "read") == 0) return op_read();
  if (strcmp(op, "write_home") == 0) return op_write_home();
  if (strcmp(op, "write_scratch") == 0) return op_write_scratch();
  if (strcmp(op, "net") == 0) return op_net();
  if (strcmp(op, "exec") == 0) return op_exec();
  fprintf(stderr, "unknown op: %s\n", op);
  return 3;
}
