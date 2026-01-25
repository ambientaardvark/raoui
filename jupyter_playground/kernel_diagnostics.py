"""
Diagnostic script to understand ark kernel message flow.
Runs various scenarios and logs the exact iopub message sequence.
"""

import json
import subprocess
import uuid
import hmac
import hashlib
import secrets
import time
import signal
import os
from datetime import datetime, timezone

import zmq

KERNEL_PATH = '/Users/alanlee/Documents/Programs/raoui/vendor/ark-0.1.223-darwin-universal/ark'
CONNECTION_FILE = '/tmp/kernel.json'


class Kernel:
    def __init__(self):
        self.session_id = str(uuid.uuid4())
        self.ctx = zmq.Context()
        self.shell = None
        self.iopub = None
        self.key = None
        self.proc = None

    def start(self):
        conn_info = {
            'ip': '127.0.0.1',
            'transport': 'tcp',
            'shell_port': 57001,
            'iopub_port': 57002,
            'stdin_port': 57003,
            'control_port': 57004,
            'hb_port': 57005,
            'key': secrets.token_hex(16),
            'signature_scheme': 'hmac-sha256',
            'kernel_name': 'ark'
        }
        self.key = conn_info['key']

        with open(CONNECTION_FILE, 'w') as f:
            json.dump(conn_info, f)

        self.proc = subprocess.Popen(
            [KERNEL_PATH, '--connection_file', CONNECTION_FILE, '--session-mode', 'notebook'],
            stdout=subprocess.DEVNULL,
            stderr=subprocess.DEVNULL
        )

        self.shell = self.ctx.socket(zmq.DEALER)
        self.shell.connect(f"tcp://127.0.0.1:{conn_info['shell_port']}")

        self.iopub = self.ctx.socket(zmq.SUB)
        self.iopub.connect(f"tcp://127.0.0.1:{conn_info['iopub_port']}")
        self.iopub.setsockopt(zmq.SUBSCRIBE, b'')

        self._wait_for_ready()

    def _wait_for_ready(self):
        self._send('kernel_info_request', {})
        while True:
            msg = self._recv(self.shell, timeout_ms=5000)
            if msg and msg['header']['msg_type'] == 'kernel_info_reply':
                break
        while self._recv(self.iopub, timeout_ms=100):
            pass

    def _make_header(self, msg_type):
        return {
            'msg_id': str(uuid.uuid4()),
            'msg_type': msg_type,
            'username': 'user',
            'session': self.session_id,
            'version': '5.4',
            'date': datetime.now(timezone.utc).isoformat()
        }

    def _sign(self, parts):
        if not self.key:
            return b''
        h = hmac.new(self.key.encode('utf-8'), digestmod=hashlib.sha256)
        for part in parts:
            h.update(part)
        return h.hexdigest().encode('utf-8')

    def _send(self, msg_type, content):
        header = self._make_header(msg_type)
        header_b = json.dumps(header).encode('utf-8')
        parent_b = b'{}'
        metadata_b = b'{}'
        content_b = json.dumps(content).encode('utf-8')

        signature = self._sign([header_b, parent_b, metadata_b, content_b])

        self.shell.send_multipart([
            b'', b'<IDS|MSG>', signature,
            header_b, parent_b, metadata_b, content_b
        ])

    def _recv(self, socket, timeout_ms=1000):
        if socket.poll(timeout_ms) == 0:
            return None
        parts = socket.recv_multipart()
        try:
            delim_idx = parts.index(b'<IDS|MSG>')
        except ValueError:
            return None
        return {
            'header': json.loads(parts[delim_idx + 2]),
            'parent_header': json.loads(parts[delim_idx + 3]),
            'metadata': json.loads(parts[delim_idx + 4]),
            'content': json.loads(parts[delim_idx + 5]),
            'buffers': parts[delim_idx + 6:]
        }

    def drain_all(self):
        """Drain any pending messages from both sockets."""
        drained = 0
        while self._recv(self.iopub, timeout_ms=50):
            drained += 1
        while self._recv(self.shell, timeout_ms=50):
            drained += 1
        return drained

    def execute_and_log(self, code, timeout_ms=10000):
        """
        Execute code and return a log of all iopub messages received.
        Returns list of (timestamp_ms, msg_type, content) tuples.
        """
        # Drain any leftover messages first
        self.drain_all()

        self._send('execute_request', {
            'code': code,
            'silent': False,
            'store_history': True,
            'user_expressions': {},
            'allow_stdin': False,
            'stop_on_error': True
        })

        start_time = time.time()
        messages = []
        saw_busy = False

        while (time.time() - start_time) * 1000 < timeout_ms:
            msg = self._recv(self.iopub, timeout_ms=100)
            if msg:
                elapsed_ms = (time.time() - start_time) * 1000
                msg_type = msg['header']['msg_type']
                content = msg['content']
                messages.append((elapsed_ms, msg_type, content))

                if msg_type == 'status':
                    if content['execution_state'] == 'busy':
                        saw_busy = True
                    elif content['execution_state'] == 'idle' and saw_busy:
                        break

            # Also drain shell socket (but don't log it for now)
            self._recv(self.shell, timeout_ms=10)

        # Note if we timed out
        if (time.time() - start_time) * 1000 >= timeout_ms:
            messages.append((timeout_ms, ">>> TIMEOUT <<<", {}))

        return messages

    def interrupt(self):
        """Send SIGINT to the kernel."""
        if self.proc:
            self.proc.send_signal(signal.SIGINT)

    def execute_with_interrupt(self, code, interrupt_after_ms=500, timeout_ms=10000):
        """
        Execute code but send SIGINT after a delay.
        Returns log of all iopub messages.
        """
        # Drain any leftover messages first
        self.drain_all()

        self._send('execute_request', {
            'code': code,
            'silent': False,
            'store_history': True,
            'user_expressions': {},
            'allow_stdin': False,
            'stop_on_error': True
        })

        start_time = time.time()
        messages = []
        saw_busy = False
        interrupted = False

        while (time.time() - start_time) * 1000 < timeout_ms:
            elapsed_ms = (time.time() - start_time) * 1000

            # Send interrupt after delay
            if not interrupted and elapsed_ms > interrupt_after_ms:
                self.interrupt()
                interrupted = True
                messages.append((elapsed_ms, ">>> SIGINT SENT <<<", {}))

            msg = self._recv(self.iopub, timeout_ms=100)
            if msg:
                msg_type = msg['header']['msg_type']
                content = msg['content']
                messages.append(((time.time() - start_time) * 1000, msg_type, content))

                if msg_type == 'status':
                    if content['execution_state'] == 'busy':
                        saw_busy = True
                    elif content['execution_state'] == 'idle' and saw_busy:
                        break

            self._recv(self.shell, timeout_ms=10)

        if (time.time() - start_time) * 1000 >= timeout_ms:
            messages.append((timeout_ms, ">>> TIMEOUT <<<", {}))

        return messages

    def shutdown(self):
        if self.proc:
            self.proc.terminate()
            self.proc.wait()


def print_message_log(messages):
    """Pretty print a message log."""
    for elapsed_ms, msg_type, content in messages:
        # Handle special markers
        if msg_type.startswith('>>>'):
            print(f"  {elapsed_ms:7.1f}ms  {msg_type}")
            continue

        # Summarize content based on message type
        base_type = msg_type.replace('[shell] ', '')
        if base_type == 'status':
            summary = content['execution_state']
        elif base_type == 'stream':
            text = content['text'].replace('\n', '\\n')
            if len(text) > 50:
                text = text[:50] + '...'
            summary = f"{content['name']}: {repr(text)}"
        elif base_type == 'execute_result':
            data = content.get('data', {})
            text = data.get('text/plain', str(data))
            if len(text) > 50:
                text = text[:50] + '...'
            summary = repr(text)
        elif base_type == 'display_data':
            data = content.get('data', {})
            mime_types = list(data.keys())
            summary = f"mime types: {mime_types}"
        elif base_type == 'error':
            summary = f"{content.get('ename', '?')}: {content.get('evalue', '?')}"
        elif base_type == 'execute_input':
            code = content.get('code', '')
            if len(code) > 30:
                code = code[:30] + '...'
            summary = repr(code)
        elif base_type == 'comm_open':
            summary = f"target: {content.get('target_name', '?')}"
        elif base_type == 'comm_msg':
            summary = f"comm_id: {content.get('comm_id', '?')[:16]}..."
        elif base_type == 'input_request':
            summary = f"prompt: {content.get('prompt', '?')}"
        elif base_type == 'execute_reply':
            summary = f"status: {content.get('status', '?')}"
        else:
            summary = str(content)[:60]

        print(f"  {elapsed_ms:7.1f}ms  {msg_type:25s}  {summary}")


def run_scenario(kernel, name, code, interrupt_after_ms=None):
    """Run a scenario and print results."""
    print(f"\n{'='*60}")
    print(f"SCENARIO: {name}")
    print(f"CODE: {repr(code)}")
    if interrupt_after_ms:
        print(f"INTERRUPT AFTER: {interrupt_after_ms}ms")
    print(f"{'='*60}")

    if interrupt_after_ms:
        messages = kernel.execute_with_interrupt(code, interrupt_after_ms=interrupt_after_ms)
    else:
        messages = kernel.execute_and_log(code)
    print_message_log(messages)

    # Summary
    msg_types = [m[1] for m in messages]
    print(f"\nMessage sequence: {' -> '.join(msg_types)}")


if __name__ == '__main__':
    print("Starting kernel...")
    kernel = Kernel()
    kernel.start()
    print("Kernel ready.\n")

    scenarios = [
        ("Simple expression", "1 + 1"),
        ("Print statement", 'print("hello")'),
        ("Multiple prints", 'print("one"); print("two"); print("three")'),
        ("Expression with print", 'print("side effect"); 42'),
        ("Syntax error", "1 +"),
        ("Runtime error (stop)", 'stop("oops")'),
        ("Runtime error (undefined var)", "undefined_variable"),
        ("Long-running (1 sec)", "Sys.sleep(1); 'done'"),
        ("Long-running (2 sec)", "Sys.sleep(2); 'done'"),
        ("Multi-output loop", "for(i in 1:3) print(i)"),
        ("Cat output", 'cat("hello", "world", sep="-")'),
        ("Warning", "warning('be careful'); 123"),
        ("Multiple expressions", "x <- 10\ny <- 20\nx + y"),
        # Large output
        ("Large output (1000 numbers)", "1:1000"),
        ("Large output (print)", "print(1:500)"),
        ("Very large string", 'paste(rep("x", 10000), collapse="")'),
        # Plots/graphics
        ("Simple plot", "plot(1:10)"),
        ("Histogram", "hist(rnorm(100))"),
        ("ggplot (if available)", "if(require(ggplot2, quietly=TRUE)) ggplot(mtcars, aes(mpg)) + geom_histogram() else 'ggplot2 not installed'"),
        # Message/cat to stderr
        ("Message to stderr", "message('this is a message'); 'done'"),
        # Invisible return
        ("Invisible return", "invisible(42)"),
        ("Invisible with print", "x <- invisible(42); print('printed'); x"),
        # NULL result
        ("NULL result", "NULL"),
        ("Function with no return", "f <- function() { 1 + 1 }; f()"),
        # Complex objects
        ("Data frame", "data.frame(a=1:3, b=c('x','y','z'))"),
        ("List", "list(a=1, b='hello', c=1:5)"),
        # Multiple errors
        ("Error after output", "print('before'); stop('boom'); print('after')"),
    ]

    for name, code in scenarios:
        run_scenario(kernel, name, code)

    # Interrupt scenarios (run separately since they're timing-sensitive)
    print("\n" + "="*60)
    print("INTERRUPT SCENARIOS")
    print("="*60)

    interrupt_scenarios = [
        ("Interrupt during sleep", "Sys.sleep(5); 'should not see this'", 500),
        ("Interrupt during loop", "for(i in 1:1000000) { x <- i * 2 }; 'done'", 200),
    ]

    for name, code, interrupt_ms in interrupt_scenarios:
        run_scenario(kernel, name, code, interrupt_after_ms=interrupt_ms)

    # Stdin scenario - run last with short timeout since it will block
    # Note: This will timeout because readline waits for input
    print("\n" + "="*60)
    print("STDIN SCENARIOS (expect timeout)")
    print("="*60)

    # Use a shorter timeout for readline since it will hang
    print("\n" + "="*60)
    print("SCENARIO: Readline (blocks waiting for stdin)")
    print("CODE: 'readline(\"prompt: \")'")
    print("="*60)
    # Send with allow_stdin=True to see what messages we get
    kernel.drain_all()
    kernel._send('execute_request', {
        'code': 'readline("prompt: ")',
        'silent': False,
        'store_history': True,
        'user_expressions': {},
        'allow_stdin': True,  # Enable stdin
        'stop_on_error': True
    })
    start = time.time()
    messages = []
    while (time.time() - start) < 2:  # 2 second timeout
        msg = kernel._recv(kernel.iopub, timeout_ms=100)
        if msg:
            elapsed = (time.time() - start) * 1000
            messages.append((elapsed, msg['header']['msg_type'], msg['content']))
        # Check shell too
        shell_msg = kernel._recv(kernel.shell, timeout_ms=100)
        if shell_msg:
            elapsed = (time.time() - start) * 1000
            messages.append((elapsed, f"[shell] {shell_msg['header']['msg_type']}", shell_msg['content']))

    print_message_log(messages)
    msg_types = [m[1] for m in messages]
    print(f"\nMessage sequence: {' -> '.join(msg_types)}")

    # Send interrupt to unblock the readline
    print("\n(Sending SIGINT to unblock readline...)")
    kernel.interrupt()
    time.sleep(0.5)
    kernel.drain_all()

    print("\n" + "="*60)
    print("All scenarios complete.")
    print("="*60)

    kernel.shutdown()
