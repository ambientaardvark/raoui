type t = {
  bytes : string;
  offsets : int array; (* byte offset where each cluster starts *)
  widths : int array; (* display width of each cluster *)
}

type error = Invalid_utf8 | Leading_combiner

let empty = { bytes = ""; offsets = [||]; widths = [||] }
let length t = Array.length t.offsets
let display_width t = Array.fold_left ( + ) 0 t.widths
let byte_length t = String.length t.bytes
let is_empty t = String.length t.bytes = 0
let to_string t = t.bytes

let cluster_at t i =
  let start = t.offsets.(i) in
  let stop =
    if i + 1 < Array.length t.offsets then t.offsets.(i + 1)
    else String.length t.bytes
  in
  String.sub t.bytes start (stop - start)

let width_at t i = t.widths.(i)

let byte_range_at t i =
  assert (i >= 0);
  assert (i < length t);
  let start = t.offsets.(i) in
  let stop =
    if i + 1 < length t then t.offsets.(i + 1) else String.length t.bytes
  in
  (start, stop)

let slice_bytes t ~start_byte ~end_byte =
  assert (start_byte >= 0);
  assert (end_byte >= start_byte);
  assert (end_byte <= String.length t.bytes);
  String.sub t.bytes start_byte (end_byte - start_byte)

let prefix_width t n =
  let len = min n (length t) in
  let rec sum acc i =
    if i >= len then acc else sum (acc + t.widths.(i)) (i + 1)
  in
  sum 0 0

let grapheme_at_width t target_width =
  let len = length t in
  let rec find i w =
    if i >= len then len
    else
      let next_w = w + t.widths.(i) in
      if next_w > target_width then i else find (i + 1) next_w
  in
  find 0 0

(* Compute display width of a single codepoint *)
let uchar_width u =
  match Uucp.Break.tty_width_hint u with
  | -1 -> 0 (* control chars, combining marks *)
  | n -> n

(* Check if codepoint is a combining mark (grapheme extends) *)
let is_combiner u =
  match Uucp.Gc.general_category u with
  | `Mn | `Mc | `Me -> true (* Mark: nonspacing, spacing combining, enclosing *)
  | _ -> false

let of_string s =
  if String.length s = 0 then Ok empty
  else
    let offsets = Dynarray.create () in
    let widths = Dynarray.create () in

    (* Decode UTF-8 and collect codepoints with byte positions *)
    let decoder = Uutf.decoder ~encoding:`UTF_8 (`String s) in
    let codepoints = Dynarray.create () in

    let rec decode () =
      match Uutf.decode decoder with
      | `Uchar u ->
          let pos =
            Uutf.decoder_byte_count decoder - Uchar.utf_8_byte_length u
          in
          Dynarray.add_last codepoints (u, pos);
          decode ()
      | `End -> Ok ()
      | `Malformed _ -> Error Invalid_utf8
      | `Await -> assert false (* not using manual source *)
    in

    match decode () with
    | Error e -> Error e
    | Ok () ->
        if Dynarray.length codepoints = 0 then Ok empty
        else
          (* Check for leading combining mark *)
          let first_uchar, _ = Dynarray.get codepoints 0 in
          if is_combiner first_uchar then Error Leading_combiner
          else begin
            (* Segment into grapheme clusters using uuseg *)
            let segmenter = Uuseg.create `Grapheme_cluster in

            let current_cluster_start = ref 0 in
            let current_cluster_width = ref 0 in
            let cp_idx = ref 0 in

            let add_boundary () =
              if !cp_idx > 0 then begin
                Dynarray.add_last offsets !current_cluster_start;
                Dynarray.add_last widths !current_cluster_width;
                current_cluster_width := 0;
                if !cp_idx < Dynarray.length codepoints then begin
                  let _, pos = Dynarray.get codepoints !cp_idx in
                  current_cluster_start := pos
                end
              end
            in

            (* Drain any pending output from segmenter *)
            let rec drain () =
              match Uuseg.add segmenter `Await with
              | `Boundary ->
                  add_boundary ();
                  drain ()
              | `Uchar _ -> drain ()
              | `Await -> ()
              | `End -> ()
            in

            (* Feed codepoints to segmenter *)
            for i = 0 to Dynarray.length codepoints - 1 do
              let u, pos = Dynarray.get codepoints i in
              if i = 0 then current_cluster_start := pos;

              (* Add codepoint and drain any output *)
              (match Uuseg.add segmenter (`Uchar u) with
              | `Boundary ->
                  add_boundary ();
                  drain ()
              | `Uchar _ -> drain ()
              | `Await -> ()
              | `End -> ());

              (* Accumulate display width: use max width within cluster *)
              let w = uchar_width u in
              if w > !current_cluster_width then current_cluster_width := w;
              cp_idx := i + 1
            done;

            (* Signal end and drain final output *)
            let rec finish_drain () =
              match Uuseg.add segmenter `Await with
              | `Boundary ->
                  add_boundary ();
                  finish_drain ()
              | `Uchar _ -> finish_drain ()
              | `Await -> ()
              | `End -> ()
            in
            (match Uuseg.add segmenter `End with
            | `Boundary ->
                add_boundary ();
                finish_drain ()
            | `Uchar _ -> finish_drain ()
            | `Await -> ()
            | `End -> ());

            (* Add final cluster if we have remaining content *)
            if !current_cluster_width > 0 then begin
              Dynarray.add_last offsets !current_cluster_start;
              Dynarray.add_last widths !current_cluster_width
            end;

            Ok
              {
                bytes = s;
                offsets = Dynarray.to_array offsets;
                widths = Dynarray.to_array widths;
              }
          end

let is_single_ascii_width1 s =
  String.length s = 1
  &&
  match String.get s 0 with
  | '\x00' .. '\x1f' | '\x7f' -> false
  | '\x00' .. '\x7f' -> true
  | _ -> false

let insert_ascii_width1 t ~pos str =
  assert (is_single_ascii_width1 str);
  assert (pos >= 0);
  let len = length t in
  let pos = min pos len in
  let byte_pos = if pos = len then String.length t.bytes else t.offsets.(pos) in
  let bytes_len = String.length t.bytes in
  let new_bytes = Bytes.create (bytes_len + 1) in
  Bytes.blit_string t.bytes 0 new_bytes 0 byte_pos;
  Bytes.blit_string str 0 new_bytes byte_pos 1;
  Bytes.blit_string t.bytes byte_pos new_bytes (byte_pos + 1)
    (bytes_len - byte_pos);
  let new_offsets = Array.make (len + 1) 0 in
  Array.blit t.offsets 0 new_offsets 0 pos;
  new_offsets.(pos) <- byte_pos;
  for i = pos to len - 1 do
    new_offsets.(i + 1) <- t.offsets.(i) + 1
  done;
  let new_widths = Array.make (len + 1) 1 in
  Array.blit t.widths 0 new_widths 0 pos;
  Array.blit t.widths pos new_widths (pos + 1) (len - pos);
  {
    bytes = Bytes.unsafe_to_string new_bytes;
    offsets = new_offsets;
    widths = new_widths;
  }

let insert_string t ~pos str =
  if pos >= 0 && is_single_ascii_width1 str then
    Ok (insert_ascii_width1 t ~pos str)
  else
    match of_string str with
    | Error e -> Error e
    | Ok _ -> (
        let len = length t in
        let byte_pos =
          if pos >= len then String.length t.bytes else t.offsets.(pos)
        in
        let after_bytes =
          String.sub t.bytes byte_pos (String.length t.bytes - byte_pos)
        in
        (* Only parse inserted + after, reuse everything before *)
        match of_string (str ^ after_bytes) with
        | Error _ -> Error Invalid_utf8
        | Ok suffix ->
            let prefix_len = min pos len in
            Ok
              {
                bytes = String.sub t.bytes 0 byte_pos ^ suffix.bytes;
                offsets =
                  Array.concat
                    [
                      Array.sub t.offsets 0 prefix_len;
                      Array.map (fun o -> o + byte_pos) suffix.offsets;
                    ];
                widths =
                  Array.concat
                    [ Array.sub t.widths 0 prefix_len; suffix.widths ];
              })

let delete t pos =
  if pos < 0 || pos >= length t then t
  else if t.widths.(pos) = 1 then
    let start = t.offsets.(pos) in
    let stop =
      if pos + 1 < length t then t.offsets.(pos + 1) else String.length t.bytes
    in
    if stop - start = 1 && Char.code t.bytes.[start] < 0x80 then (
      let len = length t in
      let bytes_len = String.length t.bytes in
      let new_bytes = Bytes.create (bytes_len - 1) in
      Bytes.blit_string t.bytes 0 new_bytes 0 start;
      Bytes.blit_string t.bytes stop new_bytes start (bytes_len - stop);
      let new_offsets = Array.make (len - 1) 0 in
      Array.blit t.offsets 0 new_offsets 0 pos;
      for i = pos + 1 to len - 1 do
        new_offsets.(i - 1) <- t.offsets.(i) - 1
      done;
      let new_widths = Array.make (len - 1) 0 in
      Array.blit t.widths 0 new_widths 0 pos;
      Array.blit t.widths (pos + 1) new_widths pos (len - pos - 1);
      {
        bytes = Bytes.unsafe_to_string new_bytes;
        offsets = new_offsets;
        widths = new_widths;
      })
    else
      let before = String.sub t.bytes 0 start in
      let after = String.sub t.bytes stop (String.length t.bytes - stop) in
      match of_string (before ^ after) with Ok t' -> t' | Error _ -> t
  else
    let start = t.offsets.(pos) in
    let stop =
      if pos + 1 < length t then t.offsets.(pos + 1) else String.length t.bytes
    in
    let before = String.sub t.bytes 0 start in
    let after = String.sub t.bytes stop (String.length t.bytes - stop) in
    match of_string (before ^ after) with
    | Ok t' -> t'
    | Error _ -> t (* shouldn't happen with valid input *)

let delete_range t ~start ~len =
  if start < 0 || len <= 0 || start >= length t then t
  else
    let actual_len = min len (length t - start) in
    let byte_start = t.offsets.(start) in
    let byte_stop =
      if start + actual_len >= length t then String.length t.bytes
      else t.offsets.(start + actual_len)
    in
    let before = String.sub t.bytes 0 byte_start in
    let after =
      String.sub t.bytes byte_stop (String.length t.bytes - byte_stop)
    in
    match of_string (before ^ after) with Ok t' -> t' | Error _ -> t

let sub t ~start ~len =
  if start < 0 || len <= 0 || start >= length t then empty
  else
    let actual_len = min len (length t - start) in
    let byte_start = t.offsets.(start) in
    let byte_stop =
      if start + actual_len >= length t then String.length t.bytes
      else t.offsets.(start + actual_len)
    in
    {
      bytes = String.sub t.bytes byte_start (byte_stop - byte_start);
      offsets =
        Array.init actual_len (fun i -> t.offsets.(start + i) - byte_start);
      widths = Array.sub t.widths start actual_len;
    }

let append a b =
  if is_empty a then b
  else if is_empty b then a
  else
    match of_string (a.bytes ^ b.bytes) with
    | Ok t -> t
    | Error _ -> a (* shouldn't happen with valid inputs *)

let concat ts =
  let combined = String.concat "" (List.map to_string ts) in
  match of_string combined with Ok t -> t | Error _ -> empty

let split t pos =
  if pos <= 0 then (empty, t)
  else if pos >= length t then (t, empty)
  else
    let before = sub t ~start:0 ~len:pos in
    let after = sub t ~start:pos ~len:(length t - pos) in
    (before, after)

let wrap t ~width =
  if width <= 0 then [ t ]
  else if is_empty t then [ empty ]
  else
    let len = length t in
    let rec loop acc start =
      if start >= len then List.rev acc
      else
        (* Find how many graphemes fit *)
        let rec find_end i w =
          if i >= len then i
          else
            let char_w = t.widths.(i) in
            if w + char_w > width then i else find_end (i + 1) (w + char_w)
        in
        let end_idx = find_end start 0 in
        if end_idx = start then
          (* Single wide char exceeds width - take it anyway *)
          let chunk = sub t ~start ~len:1 in
          loop (chunk :: acc) (start + 1)
        else
          let chunk = sub t ~start ~len:(end_idx - start) in
          loop (chunk :: acc) end_idx
    in
    let wrapped = loop [] 0 in
    (* Add empty string when line exactly fills width, for cursor positioning *)
    let total_width = display_width t in
    if total_width > 0 && total_width mod width = 0 then wrapped @ [ empty ]
    else wrapped
