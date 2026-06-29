open Raoui
module Term = Terminal_ops.Ansi
module V = View.Make (Term)

let us s =
  match Unicode_string.of_string s with
  | Ok u -> u
  | Error _ -> failwith ("invalid unicode string: " ^ s)

let repeat_string n s =
  let buf = Buffer.create (n * String.length s) in
  for _ = 1 to n do
    Buffer.add_string buf s
  done;
  Buffer.contents buf

let bench_width = 80

let make_model ?(width = bench_width) ?(height = 24) ?(cursor_line = 0)
    ?cursor_pos lines =
  let cursor_pos =
    match cursor_pos with
    | Some pos -> pos
    | None -> Unicode_string.length (List.nth lines cursor_line)
  in
  {
    Frontend_types.input =
      {
        lines;
        lex_cache = R_lex_cache.create lines;
        cursor_line;
        cursor_pos;
        previous_key = None;
        persistent_col = cursor_pos;
        history = History.init "/dev/null";
        flipping_through_history = None;
        completion = None;
        mode = Frontend_types.Normal;
      };
    layout =
      {
        prompt_top_row = 1;
        term_width = width;
        term_height = height;
        prompt_box_height = 5;
        previous_prompt_top_row = 1;
        scroll_amount = 0;
        running_in_ide = false;
      };
    repl =
      {
        awaiting_response = false;
        backend_response = None;
        repl_output = None;
        pending_suggestion = None;
        repl_cursor = (10, 1);
      };
    theme = Theme.tokyo_night;
  }

type bench = {
  group : string;
  name : string;
  iterations : int;
  run : unit -> unit;
}

let iteration_divisor =
  match Sys.getenv_opt "RAOUI_BENCH_DIVISOR" with
  | None -> 5
  | Some value -> (
      match int_of_string_opt value with
      | Some n when n > 0 -> n
      | _ -> failwith "RAOUI_BENCH_DIVISOR must be a positive integer")

let stat_words stat =
  stat.Gc.minor_words +. stat.major_words -. stat.promoted_words

let measure { group; name; iterations; run } =
  let iterations = max 1 (iterations / iteration_divisor) in
  Gc.compact ();
  let before_stat = Gc.quick_stat () in
  let before_time = Unix.gettimeofday () in
  try
    for _ = 1 to iterations do
      run ()
    done;
    let after_time = Unix.gettimeofday () in
    let after_stat = Gc.quick_stat () in
    let elapsed = after_time -. before_time in
    let per_iter_us = elapsed *. 1_000_000. /. float_of_int iterations in
    let words =
      (stat_words after_stat -. stat_words before_stat)
      /. float_of_int iterations
    in
    Printf.printf "%-14s %-32s %10d %12.3f %14.1f\n%!" group name iterations
      per_iter_us words
  with exn ->
    Printf.printf "%-14s %-32s %10d %12s %14s  %s\n%!" group name iterations
      "FAILED" "-" (Printexc.to_string exn)

let unicode_benches () =
  let ascii_80 = repeat_string 10 "abcdefgh " in
  let ascii_1k = repeat_string 125 "abcdefgh " in
  let ascii_5k = repeat_string 625 "abcdefgh " in
  let cjk_1k = repeat_string 250 "漢字かなカナ" in
  let combining_1k = repeat_string 250 "e\u{0301}a\u{0302}" in
  let emoji_zwj = repeat_string 100 "👨‍👩‍👧‍👦🙂" in
  let ascii_line = us ascii_1k in
  let ascii_len = Unicode_string.length ascii_line in
  let unicode_line = us (cjk_1k ^ emoji_zwj) in
  let unicode_len = Unicode_string.length unicode_line in
  let mk name iterations run = { group = "unicode"; name; iterations; run } in
  [
    mk "of_string ascii 80" 100_000 (fun () ->
        ignore (Sys.opaque_identity (Unicode_string.of_string ascii_80)));
    mk "of_string ascii 1k" 20_000 (fun () ->
        ignore (Sys.opaque_identity (Unicode_string.of_string ascii_1k)));
    mk "of_string ascii 5k" 5_000 (fun () ->
        ignore (Sys.opaque_identity (Unicode_string.of_string ascii_5k)));
    mk "of_string cjk 1k" 10_000 (fun () ->
        ignore (Sys.opaque_identity (Unicode_string.of_string cjk_1k)));
    mk "of_string combining 1k" 10_000 (fun () ->
        ignore (Sys.opaque_identity (Unicode_string.of_string combining_1k)));
    mk "of_string emoji zwj" 10_000 (fun () ->
        ignore (Sys.opaque_identity (Unicode_string.of_string emoji_zwj)));
    mk "insert ascii start 1k" 50_000 (fun () ->
        ignore
          (Sys.opaque_identity
             (Unicode_string.insert_string ascii_line ~pos:0 "x")));
    mk "insert ascii middle 1k" 50_000 (fun () ->
        ignore
          (Sys.opaque_identity
             (Unicode_string.insert_string ascii_line ~pos:(ascii_len / 2) "x")));
    mk "insert ascii end 1k" 50_000 (fun () ->
        ignore
          (Sys.opaque_identity
             (Unicode_string.insert_string ascii_line ~pos:ascii_len "x")));
    mk "delete ascii middle 1k" 50_000 (fun () ->
        ignore
          (Sys.opaque_identity
             (Unicode_string.delete ascii_line (ascii_len / 2))));
    mk "insert unicode middle" 20_000 (fun () ->
        ignore
          (Sys.opaque_identity
             (Unicode_string.insert_string unicode_line ~pos:(unicode_len / 2)
                "π")));
    mk "delete unicode middle" 20_000 (fun () ->
        ignore
          (Sys.opaque_identity
             (Unicode_string.delete unicode_line (unicode_len / 2))));
    mk "wrap ascii 1k width 80" 50_000 (fun () ->
        ignore (Sys.opaque_identity (Unicode_string.wrap ascii_line ~width:80)));
    mk "wrap unicode width 80" 20_000 (fun () ->
        ignore
          (Sys.opaque_identity (Unicode_string.wrap unicode_line ~width:80)));
  ]

let pipeline_benches () =
  let short_lines =
    [
      us "first line here";
      us "second line content";
      us "third line of text";
      us "fourth line input";
      us "fifth and final line";
    ]
  in
  let long_ascii = us (repeat_string 125 "abcdefgh ") in
  let long_unicode = us (repeat_string 100 "alpha <- '👨‍👩‍👧‍👦 π 漢字' ") in
  let many_lines =
    List.init 40 (fun i ->
        us (Printf.sprintf "line_%02d <- function(x) x + %d # comment" i i))
  in
  let short_model =
    make_model ~cursor_line:1
      ~cursor_pos:(Unicode_string.length (List.nth short_lines 1) / 2)
      short_lines
  in
  let long_mid =
    make_model ~cursor_pos:(Unicode_string.length long_ascii / 2) [ long_ascii ]
  in
  let long_end =
    make_model ~cursor_pos:(Unicode_string.length long_ascii) [ long_ascii ]
  in
  let unicode_mid =
    make_model
      ~cursor_pos:(Unicode_string.length long_unicode / 2)
      [ long_unicode ]
  in
  let wrapped_model =
    make_model ~width:40
      ~cursor_pos:(Unicode_string.length long_ascii / 2)
      [ long_ascii ]
  in
  let many_line_model =
    make_model ~height:24 ~cursor_line:20 ~cursor_pos:10 many_lines
  in
  let update key model =
    let m, _ = Update.update (Update.Key key) model in
    ignore (Sys.opaque_identity m)
  in
  let update_view key model =
    let m, _ = Update.update (Update.Key key) model in
    ignore (Sys.opaque_identity (V.view m))
  in
  let view model () = ignore (Sys.opaque_identity (V.view model)) in
  let mk group name iterations run = { group; name; iterations; run } in
  [
    mk "update" "char short middle" 100_000 (fun () ->
        update (Tty_listener.Char "x") short_model);
    mk "update" "char ascii 1k middle" 20_000 (fun () ->
        update (Tty_listener.Char "x") long_mid);
    mk "update" "char ascii 1k end" 50_000 (fun () ->
        update (Tty_listener.Char "x") long_end);
    mk "update" "char unicode middle" 10_000 (fun () ->
        update (Tty_listener.Char "π") unicode_mid);
    mk "update" "backspace ascii middle" 20_000 (fun () ->
        update Tty_listener.Backspace long_mid);
    mk "update" "right ascii 1k" 100_000 (fun () ->
        update Tty_listener.Right long_mid);
    mk "update" "down wrapped ascii" 50_000 (fun () ->
        update Tty_listener.Down wrapped_model);
    mk "update" "paste multiline 1k" 5_000 (fun () ->
        update
          (Tty_listener.Paste
             (String.concat "\n"
                (List.init 40 (fun i -> Printf.sprintf "x_%02d <- %d" i i))))
          short_model);
    mk "view" "short 5 lines" 100_000 (view short_model);
    mk "view" "ascii 1k" 20_000 (view long_mid);
    mk "view" "unicode long" 10_000 (view unicode_mid);
    mk "view" "wrapped width 40" 20_000 (view wrapped_model);
    mk "view" "many highlighted lines" 10_000 (view many_line_model);
    mk "keypress" "char short middle" 50_000 (fun () ->
        update_view (Tty_listener.Char "x") short_model);
    mk "keypress" "char ascii 1k middle" 10_000 (fun () ->
        update_view (Tty_listener.Char "x") long_mid);
    mk "keypress" "char ascii 1k end" 20_000 (fun () ->
        update_view (Tty_listener.Char "x") long_end);
    mk "keypress" "char unicode middle" 5_000 (fun () ->
        update_view (Tty_listener.Char "π") unicode_mid);
    mk "keypress" "down wrapped ascii" 20_000 (fun () ->
        update_view Tty_listener.Down wrapped_model);
  ]

let () =
  Printf.printf
    "RAOUI_BENCH_DIVISOR=%d (set to 1 for full iteration counts)\n%!"
    iteration_divisor;
  Printf.printf "%-14s %-32s %10s %12s %14s\n%!" "group" "case" "iters"
    "us/iter" "alloc words";
  Printf.printf "%s\n%!" (String.make 88 '-');
  List.iter measure (unicode_benches () @ pipeline_benches ())
