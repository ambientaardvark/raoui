open Sqlite3

type t = {
  database : db;
  mutable history : string array;
  mutable loaded_all : bool;
  mutable current_index : int;
  mutable current_prompt : Unicode_string.t list option;
}

let query_recent database =
  let stmt =
    prepare database
      "SELECT command FROM commands ORDER BY id DESC LIMIT 200"
  in
  let results = Dynarray.create () in
  let _rc =
    iter stmt ~f:(fun row ->
        match row.(0) with
        | Data.TEXT s -> Dynarray.add_last results s
        | _ -> failwith "unexpected data type in history")
  in
  Dynarray.to_array results

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

let contains_substring needle haystack =
  let needle =
    List.map Unicode_string.to_string needle |> String.concat "\n"
  in
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

let rec go_back t ?current_prompt () =
  (if t.current_index = 0 then
     match current_prompt with
     | Some p -> t.current_prompt <- Some p
     | None -> ());
  let idx = t.current_index in
  if idx >= Array.length t.history && not t.loaded_all then begin
    t.history <- query_all t.database;
    t.loaded_all <- true
  end;
  if idx < Array.length t.history then begin
    t.current_index <- idx + 1;
    if matches_prompt t.current_prompt t.history.(idx) then
      Some (to_us t.history.(idx))
    else
      go_back t ()
  end
  else None

let rec go_forwards t =
  if t.current_index <= 0 then None
  else begin
    t.current_index <- t.current_index - 1;
    if t.current_index = 0 then t.current_prompt
    else if matches_prompt t.current_prompt t.history.(t.current_index - 1) then
      Some (to_us t.history.(t.current_index - 1))
    else
      go_forwards t
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
  t.loaded_all <- false;
  t.history <- Array.append [|command|] t.history

let init file =
  let database = db_open file in
  create_table database;
  let history = query_recent database in
  { database; history; loaded_all = false; current_index = 0; current_prompt = None }
