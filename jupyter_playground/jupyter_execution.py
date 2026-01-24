import json
from contextlib import closing
from websocket import create_connection, WebSocketTimeoutException
from urllib.parse import urlparse
import requests
import uuid
import time

url_with_token = "http://localhost:8888/?token=d8c7e2286d4c71301d0f639e31446cb1039304569f95e3e3"

url = urlparse(url_with_token)._replace(query=None).geturl()
token = urlparse(url_with_token).query.split('=')[-1]
headers = {'Authorization': f'token {token}'}
requests.get(url + 'api/contents', headers=headers)

session = requests.Session()
session.headers.update(headers)
session.get(url + 'api/sessions').json()

data = session.post(
    url + 'api/sessions',
    json={'kernel': {'name': 'ark'}, 'name': 'sum.ipynb', 'path': 'sum.ipynb', 'type': 'notebook'}
).json()

kernel_id = data['kernel']['id']
session_id = data['id']


code = '1 + 1'
code_msg_id = str(uuid.uuid1())
code_msg = {
    'channel': 'shell',
    'content': {
        'code': code,
        'silent': False,
        'store_history': True,
        'user_expressions': {},
        'allow_stdin': False,
        'stop_on_error': True
    },
    'header': {
        'msg_id': str(uuid.uuid1()),
        'msg_type': 'execute_request',
        'username': '',
        'session': session_id,
        'version': '5.4'
    },
    'metadata': {},
    'parent_header': {}
}


def recv_all(conn):
    while True:
        try:
            raw = conn.recv()
            print(f"Raw: {raw[:200]}...")  # First 200 chars
            msg = json.loads(raw)
            print(f"  type: {msg['msg_type']:20} channel: {msg.get('channel', 'N/A')}")
        except WebSocketTimeoutException:
            print('timeout')
            break


ws_url = f"ws://localhost:8888/api/kernels/{kernel_id}/channels?session_id={session_id}&token={token}"

# with closing(create_connection(ws_url, timeout=10)) as conn:
#     # Initial handshake
#     kernel_info_msg = {
#         'channel': 'shell',
#         'content': {},
#         'header': {
#             'msg_id': str(uuid.uuid1()),
#             'msg_type': 'kernel_info_request',
#             'username': '',
#             'session': session_id,
#             'version': '5.4'
#         },
#         'metadata': {},
#         'parent_header': {}
#     }

#     # Now send your code
#     print('\nSending execute_request\n')
#     print(f"Sending: {json.dumps(code_msg, indent=2)}")
#     conn.send(json.dumps(code_msg))
#     conn.send(json.dumps(code_msg))
#     print('Receiving execute_reply\n')
#     recv_all(conn)

with closing(create_connection(ws_url, timeout=1)) as conn:
    # Short timeout for recv, but loop longer
    for i in range(20):  # Try for ~20 seconds
        print(f'\n--- Attempt {i} ---')
        if i == 5:  # Send execute after a few receives
            print('Sending execute_request')
            conn.send(json.dumps(code_msg))
        recv_all(conn)
        time.sleep(0.5)
