import json
import struct
import uuid
from contextlib import closing
from datetime import datetime, timezone
from urllib.parse import urlparse

import requests
from websocket import ABNF, WebSocketTimeoutException, create_connection

url_with_token = "http://localhost:8888/?token=50192fddc02caf314aa01a98578d691bce80f2e9382afbb9"

url = urlparse(url_with_token)._replace(query=None).geturl()
token = urlparse(url_with_token).query.split('=')[-1]
headers = {'Authorization': f'token {token}'}

session = requests.Session()
session.headers.update(headers)
session.get(url + 'api/sessions')

data = session.post(
    url + 'api/sessions',
    json={'kernel': {'name': 'ark'}, 'name': 'sum.ipynb', 'path': 'sum.ipynb', 'type': 'notebook'}
).json()
print(f"data: {data}")
kernel_id = data['kernel']['id']
session_id = data['id']
print(f"Kernel ID: {kernel_id}")
print(f"Session ID: {session_id}")


def serialize_binary_message(msg):
    msg_json = json.dumps(msg).encode('utf-8')
    buffers = []
    parts = [msg_json] + buffers
    nbufs = len(parts)
    header_size = 4 + 4 * nbufs
    offsets = []
    current_offset = header_size
    for part in parts:
        offsets.append(current_offset)
        current_offset += len(part)
    # Use '!' for network byte order (big-endian) - this is what jupyter_server expects
    header = struct.pack(f'!I{nbufs}I', nbufs, *offsets)
    return header + b''.join(parts)


def deserialize_binary_message(bmsg):
    # Use '!' for network byte order (big-endian)
    nbufs = struct.unpack('!I', bmsg[:4])[0]
    offsets = list(struct.unpack(f'!{nbufs}I', bmsg[4:4 + 4 * nbufs]))
    offsets.append(len(bmsg))
    parts = [bmsg[offsets[i]:offsets[i + 1]] for i in range(nbufs)]
    msg = json.loads(parts[0].decode('utf-8'))
    msg['buffers'] = parts[1:]
    return msg


def make_header(msg_type, session_id):
    return {
        'msg_id': str(uuid.uuid1()),
        'msg_type': msg_type,
        'username': '',
        'session': session_id,
        'version': '5.4',
        'date': datetime.now(timezone.utc).isoformat()
    }


def recv_all(conn):
    while True:
        try:
            opcode, raw = conn.recv_data()
            if opcode == ABNF.OPCODE_BINARY:
                msg = deserialize_binary_message(raw)
            else:
                msg = json.loads(raw)
            print(f"  type: {msg['msg_type']:20} channel: {msg.get('channel', 'N/A')}")
            print(f"  content: {msg['content']}")
            print()
        except WebSocketTimeoutException:
            print('timeout')
            break


ws_url = f"ws://localhost:8888/api/kernels/{kernel_id}/channels?session_id={session_id}&token={token}"
print(f"Connecting to: {ws_url}\n")

with closing(create_connection(ws_url, timeout=3)) as conn:
    # Initial handshake
    kernel_info_msg = {
        'channel': 'shell',
        'content': {},
        'header': make_header('kernel_info_request', session_id),
        'metadata': {},
        'parent_header': {}
    }
    conn.send(serialize_binary_message(kernel_info_msg), opcode=ABNF.OPCODE_BINARY)
    print('Sent kernel_info_request')
    recv_all(conn)

    # Execute code
    code_msg = {
        'channel': 'shell',
        'content': {
            'code': '1 + 1',
            'silent': False,
            'store_history': True,
            'user_expressions': {},
            'allow_stdin': False,
            'stop_on_error': True
        },
        'header': make_header('execute_request', session_id),
        'metadata': {},
        'parent_header': {}
    }
    conn.send(serialize_binary_message(code_msg), opcode=ABNF.OPCODE_BINARY)
    print('Sent execute_request')
    recv_all(conn)
