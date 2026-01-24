import json
import subprocess
import uuid
import hmac
import hashlib
import secrets
from datetime import datetime, timezone

import zmq

KERNEL_PATH = '/Users/alanlee/Downloads/ark-0.1.223-darwin-universal/ark'
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
        # Create connection file
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

        # Start kernel process
        self.proc = subprocess.Popen(
            [KERNEL_PATH, '--connection_file', CONNECTION_FILE, '--session-mode', 'notebook'],
            stdout=subprocess.DEVNULL,
            stderr=subprocess.DEVNULL
        )

        # Connect sockets
        self.shell = self.ctx.socket(zmq.DEALER)
        self.shell.connect(f"tcp://127.0.0.1:{conn_info['shell_port']}")

        self.iopub = self.ctx.socket(zmq.SUB)
        self.iopub.connect(f"tcp://127.0.0.1:{conn_info['iopub_port']}")
        self.iopub.setsockopt(zmq.SUBSCRIBE, b'')

        # Wait for kernel to be ready by sending kernel_info_request
        self._wait_for_ready()

    def _wait_for_ready(self):
        """Wait for kernel to be ready."""
        self._send('kernel_info_request', {})
        # Wait for kernel_info_reply on shell
        while True:
            msg = self._recv(self.shell, timeout_ms=5000)
            if msg and msg['header']['msg_type'] == 'kernel_info_reply':
                break
        # Drain any iopub messages
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

    def execute(self, code):
        """Execute code and return the result."""
        self._send('execute_request', {
            'code': code,
            'silent': False,
            'store_history': True,
            'user_expressions': {},
            'allow_stdin': False,
            'stop_on_error': True
        })

        result = None
        error = None

        # Collect messages until we get status: idle after status: busy
        saw_busy = False
        while True:
            # Check iopub first (outputs come here)
            msg = self._recv(self.iopub, timeout_ms=100)
            if msg:
                msg_type = msg['header']['msg_type']
                content = msg['content']

                if msg_type == 'status':
                    if content['execution_state'] == 'busy':
                        saw_busy = True
                    elif content['execution_state'] == 'idle' and saw_busy:
                        break
                elif msg_type == 'execute_result':
                    result = content['data'].get('text/plain')
                elif msg_type == 'stream':
                    print(content['text'], end='')
                elif msg_type == 'error':
                    error = '\n'.join(content['traceback'])
                continue

            # Check shell (execute_reply comes here)
            msg = self._recv(self.shell, timeout_ms=100)
            if msg:
                pass  # We mainly care about iopub messages for output

        if error:
            return f"Error: {error}"
        return result

    def shutdown(self):
        if self.proc:
            self.proc.terminate()
            self.proc.wait()


if __name__ == '__main__':
    import time

    print("Starting kernel...")
    start = time.time()
    kernel = Kernel()
    kernel.start()
    print(f"Kernel ready in {time.time() - start:.2f}s\n")

    # Now execution should be fast
    for expr in ['1 + 1', '2 * 3', 'sqrt(16)', 'sum(1:100)']:
        start = time.time()
        result = kernel.execute(expr)
        elapsed = time.time() - start
        print(f"{expr} = {result}  ({elapsed*1000:.0f}ms)")

    kernel.shutdown()
