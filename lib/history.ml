type t = {
  file_path : string;
  mutable history : string array;
  mutable current_index : int;
  mutable saved_prompt : Unicode_string.t list option;
  mutable search_needle : Unicode_string.t list option;
}

let collapse_empty_lines s =
  let lines = String.split_on_char '\n' s in
  let rec loop prev_empty acc = function
    | [] -> List.rev acc
    | "" :: rest ->
      if prev_empty then loop true acc rest
      else loop true ("" :: acc) rest
    | line :: rest -> loop false (line :: acc) rest
  in
  loop false [] lines |> String.concat "\n"

let parse_entries contents =
  if String.length contents = 0 then [||]
  else
    let len = String.length contents in
    let entries = Dynarray.create () in
    let buf = Buffer.create 256 in
    let blank_count = ref 0 in
    let i = ref 0 in
    while !i < len do
      let c = String.get contents !i in
      if c = '\n' then begin
        incr blank_count;
        if !blank_count >= 2 then begin
          let entry = Buffer.contents buf |> String.trim in
          if String.length entry > 0 then
            Dynarray.add_last entries entry;
          Buffer.clear buf;
          (* skip any additional blank lines *)
          while !i + 1 < len && String.get contents (!i + 1) = '\n' do
            incr i
          done
        end else
          Buffer.add_char buf '\n'
      end else begin
        blank_count := 0;
        Buffer.add_char buf c
      end;
      incr i
    done;
    let entry = Buffer.contents buf |> String.trim in
    if String.length entry > 0 then
      Dynarray.add_last entries entry;
    Dynarray.to_array entries

let max_history = 5000

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

let prompt_to_string p =
  List.map Unicode_string.to_string p |> String.concat "\n"

let prompt_matches_entry prompt idx history =
  idx > 0 && idx - 1 < Array.length history &&
  prompt_to_string prompt = history.(idx - 1)

let update_search_needle t current_prompt =
  if not (prompt_matches_entry current_prompt t.current_index t.history) then
    t.search_needle <- Some current_prompt

let is_duplicate_of_previous t idx =
  idx > 0 && t.history.(idx) = t.history.(idx - 1)

let rec go_back t ?current_prompt () =
  (match current_prompt with
   | Some p ->
     if t.current_index = 0 then
       t.saved_prompt <- Some p;
     update_search_needle t p
   | None -> ());
  let idx = t.current_index in
  if idx < Array.length t.history then begin
    t.current_index <- idx + 1;
    if is_duplicate_of_previous t idx then
      go_back t ()
    else if matches_prompt t.search_needle t.history.(idx) then
      Some (to_us t.history.(idx))
    else
      go_back t ()
  end
  else None

let rec go_forwards t ?current_prompt () =
  (match current_prompt with
   | Some p -> update_search_needle t p
   | None -> ());
  if t.current_index <= 0 then None
  else begin
    t.current_index <- t.current_index - 1;
    if t.current_index = 0 then t.saved_prompt
    else
      let idx = t.current_index - 1 in
      if is_duplicate_of_previous t idx then
        go_forwards t ()
      else if matches_prompt t.search_needle t.history.(idx) then
        Some (to_us t.history.(idx))
      else
        go_forwards t ()
  end

let get_all t = t.history

let search_history t pattern =
  let pattern = String.lowercase_ascii pattern in
  (* strip leading/trailing % from the LIKE pattern *)
  let needle =
    let p = if String.length pattern > 0 && String.get pattern 0 = '%'
      then String.sub pattern 1 (String.length pattern - 1) else pattern in
    if String.length p > 0 && String.get p (String.length p - 1) = '%'
    then String.sub p 0 (String.length p - 1) else p
  in
  if String.length needle = 0 then ""
  else
    let found = ref "" in
    let i = ref 0 in
    while !found = "" && !i < Array.length t.history do
      let entry = t.history.(!i) in
      if let lower = String.lowercase_ascii entry in
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

let close t =
  let len = Array.length t.history in
  let count = min len max_history in
  let oc = open_out t.file_path in
  Fun.protect
    (fun () ->
       for i = count - 1 downto 0 do
         let entry = collapse_empty_lines t.history.(i) in
         output_string oc entry;
         if i > 0 then output_string oc "\n\n\n"
       done;
       output_char oc '\n')
    ~finally:(fun () -> close_out oc)

let add_to_history t lines =
  let command = List.map Unicode_string.to_string lines |> String.concat "\n" in
  t.current_index <- 0;
  t.saved_prompt <- None;
  t.search_needle <- None;
  t.history <- Array.append [|command|] t.history

let init file =
  let contents =
    if Sys.file_exists file then
      In_channel.with_open_text file In_channel.input_all
    else ""
  in
  let all_entries = parse_entries contents in
  (* Reverse so newest is first, matching the convention used by navigation *)
  let history =
    let len = Array.length all_entries in
    Array.init len (fun i -> all_entries.(len - 1 - i))
  in
  { file_path = file; history; current_index = 0; saved_prompt = None; search_needle = None }
