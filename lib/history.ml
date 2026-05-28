module S = Sqlite3

type output = {
  kind : string;
  text : string option;
  image_path : string option;
}

type interaction = {
  input : string;
  mode : string;
  submitted_at : float;
  outputs : output list;
}

type t = {
  file_path : string;
  db : S.db;
  session_id : string;
  history : string Dynarray.t;
  mutable current_index : int;
  mutable saved_prompt : Unicode_string.t list option;
  mutable search_needle : Unicode_string.t list option;
  mutable active_command_id : int64 option;
  mutable active_command_had_error : bool;
  mutable active_output_ordinal : int;
}

let max_history = 5000
let now = Unix.gettimeofday

let check db rc =
  if not (S.Rc.is_success rc) then
    failwith
      (Printf.sprintf "sqlite error: %s: %s" (S.Rc.to_string rc) (S.errmsg db))

let exec db sql = check db (S.exec db sql)

let finalize stmt =
  let rc = S.finalize stmt in
  if not (S.Rc.is_success rc) then
    failwith (Printf.sprintf "sqlite finalize error: %s" (S.Rc.to_string rc))

let with_stmt db sql f =
  let stmt = S.prepare db sql in
  Fun.protect (fun () -> f stmt) ~finally:(fun () -> finalize stmt)

let bind_values db stmt values = check db (S.bind_values stmt values)

let step_done db stmt =
  let rc = S.step stmt in
  match rc with S.Rc.DONE -> () | _ -> check db rc

let to_us s =
  String.split_on_char '\n' s
  |> List.map (fun s -> Unicode_string.of_string s |> Result.get_ok)

let contains_substring needle haystack =
  let needle = List.map Unicode_string.to_string needle |> String.concat "\n" in
  let needle_len = String.length needle in
  let haystack_len = String.length haystack in
  let rec loop i =
    if i + needle_len > haystack_len then false
    else if String.sub haystack i needle_len = needle then true
    else loop (i + 1)
  in
  loop 0

let matches_prompt prompt entry =
  match prompt with
  | None -> true
  | Some needle -> contains_substring needle entry

let prompt_to_string p =
  List.map Unicode_string.to_string p |> String.concat "\n"

(* History is stored oldest-first so appending a new command is amortized O(1);
   navigation indices count back from the newest entry. *)
let nth_newest t i = Dynarray.get t.history (Dynarray.length t.history - 1 - i)

let prompt_matches_entry t prompt idx =
  idx > 0
  && idx - 1 < Dynarray.length t.history
  && prompt_to_string prompt = nth_newest t (idx - 1)

let update_search_needle t current_prompt =
  if not (prompt_matches_entry t current_prompt t.current_index) then
    t.search_needle <- Some current_prompt

let is_duplicate_of_previous t idx =
  idx > 0 && nth_newest t idx = nth_newest t (idx - 1)

let rec go_back t ?current_prompt () =
  (match current_prompt with
  | Some p ->
      if t.current_index = 0 then t.saved_prompt <- Some p;
      update_search_needle t p
  | None -> ());
  let idx = t.current_index in
  if idx < Dynarray.length t.history then begin
    t.current_index <- idx + 1;
    if is_duplicate_of_previous t idx then go_back t ()
    else if matches_prompt t.search_needle (nth_newest t idx) then
      Some (to_us (nth_newest t idx))
    else go_back t ()
  end
  else None

let rec go_forwards t ?current_prompt () =
  (match current_prompt with Some p -> update_search_needle t p | None -> ());
  if t.current_index <= 0 then None
  else begin
    t.current_index <- t.current_index - 1;
    if t.current_index = 0 then t.saved_prompt
    else
      let idx = t.current_index - 1 in
      if is_duplicate_of_previous t idx then go_forwards t ()
      else if matches_prompt t.search_needle (nth_newest t idx) then
        Some (to_us (nth_newest t idx))
      else go_forwards t ()
  end

let get_all t =
  Array.init (Dynarray.length t.history) (fun i -> nth_newest t i)

let search_history t pattern =
  let pattern = String.lowercase_ascii pattern in
  let needle =
    let p =
      if String.length pattern > 0 && String.get pattern 0 = '%' then
        String.sub pattern 1 (String.length pattern - 1)
      else pattern
    in
    if String.length p > 0 && String.get p (String.length p - 1) = '%' then
      String.sub p 0 (String.length p - 1)
    else p
  in
  if String.length needle = 0 then ""
  else
    let found = ref "" in
    let i = ref 0 in
    while !found = "" && !i < Dynarray.length t.history do
      let entry = nth_newest t !i in
      if
        let lower = String.lowercase_ascii entry in
        let nlen = String.length needle in
        let elen = String.length lower in
        let rec check j =
          if j + nlen > elen then false
          else if String.sub lower j nlen = needle then true
          else check (j + 1)
        in
        check 0
      then found := entry;
      incr i
    done;
    !found

let create_schema db =
  exec db "PRAGMA foreign_keys = ON";
  exec db "PRAGMA journal_mode = WAL";
  exec db
    {|
CREATE TABLE IF NOT EXISTS sessions (
  id TEXT PRIMARY KEY,
  started_at REAL NOT NULL,
  cwd TEXT NOT NULL,
  pid INTEGER NOT NULL
);
CREATE TABLE IF NOT EXISTS commands (
  id INTEGER PRIMARY KEY,
  session_id TEXT NOT NULL REFERENCES sessions(id),
  submitted_at REAL NOT NULL,
  mode TEXT NOT NULL,
  input TEXT NOT NULL,
  status TEXT NOT NULL CHECK (status IN ('running', 'done', 'error', 'interrupted'))
);
CREATE TABLE IF NOT EXISTS outputs (
  id INTEGER PRIMARY KEY,
  command_id INTEGER NOT NULL REFERENCES commands(id) ON DELETE CASCADE,
  ordinal INTEGER NOT NULL,
  created_at REAL NOT NULL,
  kind TEXT NOT NULL,
  text TEXT,
  image_path TEXT,
  metadata_json TEXT,
  UNIQUE(command_id, ordinal)
);
CREATE INDEX IF NOT EXISTS idx_commands_recent ON commands(id DESC);
CREATE INDEX IF NOT EXISTS idx_outputs_command ON outputs(command_id, ordinal);
|}

let add_session db session_id =
  with_stmt db
    "INSERT OR IGNORE INTO sessions(id, started_at, cwd, pid) VALUES(?, ?, ?, \
     ?)" (fun stmt ->
      bind_values db stmt
        [
          S.Data.TEXT session_id;
          S.Data.FLOAT (now ());
          S.Data.TEXT (Sys.getcwd ());
          S.Data.INT (Int64.of_int (Unix.getpid ()));
        ];
      step_done db stmt)

let command_history_from_db db =
  with_stmt db
    "SELECT input FROM commands WHERE mode IN ('r', 'shell') ORDER BY id DESC \
     LIMIT ?" (fun stmt ->
      bind_values db stmt [ S.Data.INT (Int64.of_int max_history) ];
      let rc, rows =
        S.fold stmt ~init:[] ~f:(fun acc row ->
            match row.(0) with
            | S.Data.TEXT input -> input :: acc
            | _ -> assert false)
      in
      check db rc;
      Dynarray.of_list rows)

let finish_active ?(status = "done") t =
  match t.active_command_id with
  | None -> ()
  | Some id ->
      let status = if t.active_command_had_error then "error" else status in
      with_stmt t.db "UPDATE commands SET status = ? WHERE id = ?" (fun stmt ->
          bind_values t.db stmt [ S.Data.TEXT status; S.Data.INT id ];
          step_done t.db stmt);
      t.active_command_id <- None;
      t.active_command_had_error <- false;
      t.active_output_ordinal <- 0

let close t =
  finish_active ~status:"interrupted" t;
  let closed = S.db_close t.db in
  if not closed then failwith ("failed to close sqlite history: " ^ t.file_path)

let add_to_history ?(mode = "r") t lines =
  finish_active ~status:"interrupted" t;
  let command = List.map Unicode_string.to_string lines |> String.concat "\n" in
  t.current_index <- 0;
  t.saved_prompt <- None;
  t.search_needle <- None;
  Dynarray.add_last t.history command;
  with_stmt t.db
    "INSERT INTO commands(session_id, submitted_at, mode, input, status) \
     VALUES(?, ?, ?, ?, 'running')" (fun stmt ->
      bind_values t.db stmt
        [
          S.Data.TEXT t.session_id;
          S.Data.FLOAT (now ());
          S.Data.TEXT mode;
          S.Data.TEXT command;
        ];
      step_done t.db stmt);
  t.active_command_id <- Some (S.last_insert_rowid t.db);
  t.active_command_had_error <- false;
  t.active_output_ordinal <- 0

let response_output = function
  | Ffi_backend.Stdout text -> Some ("stdout", Some text, None)
  | Ffi_backend.Result text -> Some ("result", Some text, None)
  | Ffi_backend.R_error text -> Some ("error", Some text, None)
  | Ffi_backend.Internal_error text -> Some ("internal_error", Some text, None)
  | Ffi_backend.Restarted text -> Some ("restarted", Some text, None)
  | Ffi_backend.Image image -> Some ("image", None, Some image.preview_path)
  | Ffi_backend.Done | Ffi_backend.Shutdown | Ffi_backend.Passthrough
  | Ffi_backend.Passthrough_end | Ffi_backend.Completions _
  | Ffi_backend.Readline _ ->
      None

let record_output t kind text image_path =
  match t.active_command_id with
  | None -> ()
  | Some command_id ->
      let ordinal = t.active_output_ordinal in
      t.active_output_ordinal <- ordinal + 1;
      with_stmt t.db
        "INSERT INTO outputs(command_id, ordinal, created_at, kind, text, \
         image_path, metadata_json) VALUES(?, ?, ?, ?, ?, ?, NULL)" (fun stmt ->
          bind_values t.db stmt
            [
              S.Data.INT command_id;
              S.Data.INT (Int64.of_int ordinal);
              S.Data.FLOAT (now ());
              S.Data.TEXT kind;
              S.Data.opt_text text;
              S.Data.opt_text image_path;
            ];
          step_done t.db stmt)

let record_response t response =
  (match response_output response with
  | None -> ()
  | Some (kind, text, image_path) ->
      if kind = "error" || kind = "internal_error" then
        t.active_command_had_error <- true;
      record_output t kind text image_path);
  match response with
  | Ffi_backend.Done -> finish_active t
  | Ffi_backend.Internal_error _ | Ffi_backend.Restarted _ ->
      finish_active ~status:"error" t
  | Ffi_backend.Shutdown -> finish_active ~status:"interrupted" t
  | _ -> ()

let record_cancel t = finish_active ~status:"interrupted" t

let outputs_for_command db command_id =
  with_stmt db
    "SELECT kind, text, image_path FROM outputs WHERE command_id = ? ORDER BY \
     ordinal ASC" (fun stmt ->
      bind_values db stmt [ S.Data.INT command_id ];
      let rc, rows =
        S.fold stmt ~init:[] ~f:(fun acc row ->
            let text =
              match row.(1) with
              | S.Data.TEXT s -> Some s
              | S.Data.NULL -> None
              | _ -> assert false
            in
            let image_path =
              match row.(2) with
              | S.Data.TEXT s -> Some s
              | S.Data.NULL -> None
              | _ -> assert false
            in
            match row.(0) with
            | S.Data.TEXT kind -> { kind; text; image_path } :: acc
            | _ -> assert false)
      in
      check db rc;
      List.rev rows)

let recent_interactions t ~limit =
  assert (limit >= 0);
  with_stmt t.db
    "SELECT id, input, mode, submitted_at FROM commands ORDER BY id DESC LIMIT \
     ?" (fun stmt ->
      bind_values t.db stmt [ S.Data.INT (Int64.of_int limit) ];
      let rc, rows =
        S.fold stmt ~init:[] ~f:(fun acc row ->
            match (row.(0), row.(1), row.(2), row.(3)) with
            | ( S.Data.INT id,
                S.Data.TEXT input,
                S.Data.TEXT mode,
                S.Data.FLOAT submitted_at ) ->
                let outputs = outputs_for_command t.db id in
                { input; mode; submitted_at; outputs } :: acc
            | _ -> assert false)
      in
      check t.db rc;
      rows)

let init file =
  let db =
    if file = "/dev/null" then S.db_open ":memory:" else S.db_open file
  in
  create_schema db;
  let session_id = Printf.sprintf "%d-%.6f" (Unix.getpid ()) (now ()) in
  add_session db session_id;
  let history = command_history_from_db db in
  {
    file_path = file;
    db;
    session_id;
    history;
    current_index = 0;
    saved_prompt = None;
    search_needle = None;
    active_command_id = None;
    active_command_had_error = false;
    active_output_ordinal = 0;
  }
