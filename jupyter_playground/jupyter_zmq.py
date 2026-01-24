import json
import subprocess
import time
import uuid
import hmac
import hashlib
import secrets
from datetime import datetime, timezone

import zmq

KERNEL_PATH = '/Users/alanlee/Downloads/ark-0.1.223-darwin-universal/ark'
CONNECTION_FILE = '/tmp/kernel.json'

# Create the connection file ourselves with available ports
# We pick ports and tell the kernel to use them
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

with open(CONNECTION_FILE, 'w') as f:
    json.dump(conn_info, f)

# Start the kernel directly
print("Starting kernel...")
proc = subprocess.Popen([
    KERNEL_PATH,
    '--connection_file', CONNECTION_FILE,
    '--session-mode', 'notebook'
])

# Wait for kernel to start
time.sleep(1)

print(f"Connection info: {json.dumps(conn_info, indent=2)}\n")

session_id = str(uuid.uuid4())


def make_header(msg_type):
    return {
        'msg_id': str(uuid.uuid4()),
        'msg_type': msg_type,
        'username': 'user',
        'session': session_id,
        'version': '5.4',
        'date': datetime.now(timezone.utc).isoformat()
    }


def sign(parts, key):
    """Compute HMAC signature for message parts."""
    if not key:
        return b''
    h = hmac.new(key.encode('utf-8'), digestmod=hashlib.sha256)
    for part in parts:
        h.update(part)
    return h.hexdigest().encode('utf-8')


def send_message(socket, msg_type, content, key):
    """Send a message over ZMQ in Jupyter wire format."""
    header = make_header(msg_type)
    parent_header = {}
    metadata = {}

    # Serialize each part to JSON bytes
    header_b = json.dumps(header).encode('utf-8')
    parent_header_b = json.dumps(parent_header).encode('utf-8')
    metadata_b = json.dumps(metadata).encode('utf-8')
    content_b = json.dumps(content).encode('utf-8')

    # Compute signature
    signature = sign([header_b, parent_header_b, metadata_b, content_b], key)

    # ZMQ multipart message format:
    # [identity, delimiter, signature, header, parent_header, metadata, content, buffers...]
    parts = [
        b'',                    # identity (empty for DEALER)
        b'<IDS|MSG>',           # delimiter
        signature,              # HMAC signature
        header_b,
        parent_header_b,
        metadata_b,
        content_b,
        # no buffers
    ]

    socket.send_multipart(parts)
    print(f"Sent {msg_type}")


def recv_message(socket, timeout_ms=3000):
    """Receive a message from ZMQ socket."""
    if socket.poll(timeout_ms) == 0:
        return None

    parts = socket.recv_multipart()

    # Find the delimiter
    try:
        delim_idx = parts.index(b'<IDS|MSG>')
    except ValueError:
        print(f"No delimiter found in message: {parts}")
        return None

    # Parts after delimiter: signature, header, parent_header, metadata, content, buffers...
    signature = parts[delim_idx + 1]
    header = json.loads(parts[delim_idx + 2])
    parent_header = json.loads(parts[delim_idx + 3])
    metadata = json.loads(parts[delim_idx + 4])
    content = json.loads(parts[delim_idx + 5])
    buffers = parts[delim_idx + 6:]

    return {
        'header': header,
        'parent_header': parent_header,
        'metadata': metadata,
        'content': content,
        'buffers': buffers
    }


def recv_all(socket, timeout_ms=1000):
    """Receive all pending messages."""
    while True:
        msg = recv_message(socket, timeout_ms)
        if msg is None:
            break
        print(f"  {msg['header']['msg_type']:20} content: {msg['content']}")


# Connect to ZMQ sockets
ctx = zmq.Context()

shell = ctx.socket(zmq.DEALER)
shell.connect(f"tcp://127.0.0.1:{conn_info['shell_port']}")

iopub = ctx.socket(zmq.SUB)
iopub.connect(f"tcp://127.0.0.1:{conn_info['iopub_port']}")
iopub.setsockopt(zmq.SUBSCRIBE, b'')  # Subscribe to all messages

key = conn_info.get('key', '')

print("Connected to kernel\n")

# Give iopub a moment to connect and receive any startup messages
time.sleep(0.5)
print("Initial iopub messages:")
recv_all(iopub)

# Send kernel_info_request
print("\nSending kernel_info_request...")
send_message(shell, 'kernel_info_request', {}, key)
print("Shell replies:")
recv_all(shell)
print("IOPub messages:")
recv_all(iopub)

# Execute code
print("\nSending execute_request for '1 + 1'...")
send_message(shell, 'execute_request', {
    'code': '1 + 1',
    'silent': False,
    'store_history': True,
    'user_expressions': {},
    'allow_stdin': False,
    'stop_on_error': True
}, key)

print("Shell replies:")
recv_all(shell)
print("IOPub messages:")
recv_all(iopub)

# Clean up
print("\nShutting down kernel...")
proc.terminate()
proc.wait()
print("Done")
