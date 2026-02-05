open Sqlite3

type t = {
  database : db;
  mutable first_few : string list;
  initial_first_few_length : int;
  mutable cache : string array option;
  mutable current_index : int;
  mutable current_prompt : Unicode_string.t list option;
}

let get_recent database =
  let stmt =
    prepare database "SELECT command FROM commands ORDER BY id DESC LIMIT 100"
  in
  let _rc, results =
    fold stmt ~init:[] ~f:(fun acc row ->
        match row.(0) with
        | Data.TEXT s -> s :: acc
        | _ -> failwith "unexpected data type in history")
  in
  List.rev results

let query_all database =
  let stmt = prepare database "SELECT command FROM commands ORDER BY id DESC" in
  let results = Dynarray.create () in
  let _rc =
    iter stmt ~f:(fun row ->
        match row.(0) with
        | Data.TEXT s -> Dynarray.add_last results s
        | _ -> failwith "unexpected data type in history")
  in
  Dynarray.to_array results

let create_table database =
  let rc =
    exec database
      "CREATE TABLE IF NOT EXISTS commands (\n\
      \      id INTEGER PRIMARY KEY AUTOINCREMENT,\n\
      \      command TEXT NOT NULL,\n\
      \      timestamp TEXT NOT NULL DEFAULT (datetime('now'))\n\
      \    )"
  in
  if rc <> Rc.OK then failwith (Rc.to_string rc)

let to_us s =
  String.split_on_char '\n' s
  |> List.map (fun s -> Unicode_string.of_string s |> Result.get_ok)

let get_cached_idx t idx =
  let cache =
    match t.cache with
    | Some cache -> cache
    | None ->
        let all_commands = query_all t.database in
        t.cache <- Some all_commands;
        all_commands
  in
  if idx < Array.length cache then Some cache.(idx) else None

let get_history_entry t idx =
  let first_few_length = List.length t.first_few in
  if idx < first_few_length then
    List.nth_opt t.first_few idx |> Option.map to_us
  else
    let extras = first_few_length - t.initial_first_few_length in
    get_cached_idx t (idx - extras) |> Option.map to_us

let go_back t ?current_prompt () =
  (if t.current_index = 0 then
     match current_prompt with
     | Some p -> t.current_prompt <- Some p
     | None -> ());
  let idx = t.current_index in
  match get_history_entry t idx with
  | Some _ as r ->
      t.current_index <- idx + 1;
      r
  | None -> None

let go_forwards t =
  if t.current_index <= 0 then None
  else begin
    t.current_index <- t.current_index - 1;
    if t.current_index = 0 then t.current_prompt
    else get_history_entry t (t.current_index - 1)
  end

let get_all t = query_all t.database

let close t =
  if not (db_close t.database) then failwith "failed to close history database"

let add_to_history t lines =
  let command = List.map Unicode_string.to_string lines |> String.concat "\n" in
  let stmt = prepare t.database "INSERT INTO commands (command) VALUES (?)" in
  (match bind_text stmt 1 command with
  | Rc.OK -> ()
  | rc -> failwith ("bind_text failed: " ^ Rc.to_string rc));
  (match step stmt with Rc.DONE -> () | rc -> failwith (Rc.to_string rc));
  t.current_index <- 0;
  t.current_prompt <- None;
  t.cache <- None;
  t.first_few <- command :: t.first_few

let init file =
  let database = db_open file in
  create_table database;
  let first_few = get_recent database in
  {
    database;
    first_few;
    initial_first_few_length = List.length first_few;
    cache = None;
    current_index = 0;
    current_prompt = None;
  }
