open Raoui

let us = Alcotest.testable
  (fun fmt _ -> Format.fprintf fmt "<unicode_string>")
  (fun a b -> Unicode_string.to_string a = Unicode_string.to_string b)

let _us_result = Alcotest.result us (Alcotest.of_pp (fun fmt _ -> Format.fprintf fmt "<error>"))

let ok_us s =
  match Unicode_string.of_string s with
  | Ok t -> t
  | Error _ -> Alcotest.fail ("Failed to parse: " ^ s)

(* ===== Construction ===== *)

let test_empty () =
  let t = Unicode_string.empty in
  Alcotest.(check int) "empty length" 0 (Unicode_string.length t);
  Alcotest.(check int) "empty width" 0 (Unicode_string.display_width t);
  Alcotest.(check string) "empty to_string" "" (Unicode_string.to_string t)

let test_ascii () =
  let t = ok_us "hello" in
  Alcotest.(check int) "ascii length" 5 (Unicode_string.length t);
  Alcotest.(check int) "ascii width" 5 (Unicode_string.display_width t);
  Alcotest.(check int) "ascii bytes" 5 (Unicode_string.byte_length t);
  Alcotest.(check string) "ascii roundtrip" "hello" (Unicode_string.to_string t)

let test_multibyte_utf8 () =
  (* é as single codepoint U+00E9, 2 bytes in UTF-8 *)
  let t = ok_us "café" in
  Alcotest.(check int) "café length" 4 (Unicode_string.length t);
  Alcotest.(check int) "café width" 4 (Unicode_string.display_width t);
  Alcotest.(check int) "café bytes" 5 (Unicode_string.byte_length t)

let test_wide_chars () =
  (* CJK characters are 2 columns wide *)
  let t = ok_us "中文" in
  Alcotest.(check int) "中文 length" 2 (Unicode_string.length t);
  Alcotest.(check int) "中文 width" 4 (Unicode_string.display_width t)

let test_combining_chars () =
  (* e followed by combining acute accent U+0301 = one grapheme cluster *)
  let t = ok_us "e\xCC\x81" in  (* e + U+0301 *)
  Alcotest.(check int) "e+combining length" 1 (Unicode_string.length t);
  Alcotest.(check int) "e+combining width" 1 (Unicode_string.display_width t)

let test_emoji_simple () =
  (* Simple emoji: 😀 U+1F600 *)
  let t = ok_us "\xF0\x9F\x98\x80" in
  Alcotest.(check int) "emoji length" 1 (Unicode_string.length t);
  Alcotest.(check int) "emoji width" 2 (Unicode_string.display_width t)

let test_emoji_zwj () =
  (* Family emoji: 👨‍👩‍👧 = U+1F468 U+200D U+1F469 U+200D U+1F467 *)
  let t = ok_us "\xF0\x9F\x91\xA8\xE2\x80\x8D\xF0\x9F\x91\xA9\xE2\x80\x8D\xF0\x9F\x91\xA7" in
  Alcotest.(check int) "zwj emoji length" 1 (Unicode_string.length t);
  (* Width varies by terminal, typically 2 *)
  Alcotest.(check bool) "zwj emoji width >= 2" true (Unicode_string.display_width t >= 2)

let test_flag_emoji () =
  (* Flag: 🇺🇸 = U+1F1FA U+1F1F8 (regional indicators) *)
  let t = ok_us "\xF0\x9F\x87\xBA\xF0\x9F\x87\xB8" in
  Alcotest.(check int) "flag length" 1 (Unicode_string.length t)

let test_invalid_utf8 () =
  let result = Unicode_string.of_string "\xFF\xFE" in
  Alcotest.(check bool) "invalid utf8 is error" true (Result.is_error result)

let test_leading_combiner () =
  (* String starting with combining mark *)
  let result = Unicode_string.of_string "\xCC\x81abc" in  (* U+0301 + abc *)
  Alcotest.(check bool) "leading combiner is error" true (Result.is_error result)

(* ===== Access ===== *)

let test_cluster_at () =
  let t = ok_us "héllo" in  (* h, é (precomposed), l, l, o *)
  Alcotest.(check string) "cluster 0" "h" (Unicode_string.cluster_at t 0);
  Alcotest.(check string) "cluster 1" "é" (Unicode_string.cluster_at t 1);
  Alcotest.(check string) "cluster 2" "l" (Unicode_string.cluster_at t 2)

let test_width_at () =
  let t = ok_us "a中b" in
  Alcotest.(check int) "width at 0" 1 (Unicode_string.width_at t 0);
  Alcotest.(check int) "width at 1" 2 (Unicode_string.width_at t 1);
  Alcotest.(check int) "width at 2" 1 (Unicode_string.width_at t 2)

(* ===== Modification ===== *)

let test_delete_ascii () =
  let t = ok_us "hello" in
  let t' = Unicode_string.delete t 1 in
  Alcotest.(check string) "delete index 1" "hllo" (Unicode_string.to_string t')

let test_delete_multibyte () =
  let t = ok_us "café" in
  let t' = Unicode_string.delete t 3 in  (* delete é *)
  Alcotest.(check string) "delete é" "caf" (Unicode_string.to_string t')

let test_delete_combining () =
  (* e + combining acute, then 'x' *)
  let t = ok_us "e\xCC\x81x" in
  Alcotest.(check int) "before delete length" 2 (Unicode_string.length t);
  let t' = Unicode_string.delete t 0 in  (* delete the combined e+accent *)
  Alcotest.(check string) "delete combined" "x" (Unicode_string.to_string t');
  Alcotest.(check int) "after delete length" 1 (Unicode_string.length t')

let test_delete_range () =
  let t = ok_us "hello" in
  let t' = Unicode_string.delete_range t ~start:1 ~len:3 in
  Alcotest.(check string) "delete range" "ho" (Unicode_string.to_string t')

let test_sub () =
  let t = ok_us "hello" in
  let t' = Unicode_string.sub t ~start:1 ~len:3 in
  Alcotest.(check string) "sub" "ell" (Unicode_string.to_string t')

let test_sub_multibyte () =
  let t = ok_us "中文字" in
  let t' = Unicode_string.sub t ~start:1 ~len:2 in
  Alcotest.(check string) "sub multibyte" "文字" (Unicode_string.to_string t')

let test_insert_string_middle () =
  let t = ok_us "hllo" in
  let result = Unicode_string.insert_string t ~pos:1 "e" in
  match result with
  | Ok t' -> Alcotest.(check string) "insert middle" "hello" (Unicode_string.to_string t')
  | Error _ -> Alcotest.fail "insert should succeed"

let test_insert_string_start () =
  let t = ok_us "world" in
  let result = Unicode_string.insert_string t ~pos:0 "hello " in
  match result with
  | Ok t' -> Alcotest.(check string) "insert start" "hello world" (Unicode_string.to_string t')
  | Error _ -> Alcotest.fail "insert should succeed"

let test_insert_string_end () =
  let t = ok_us "hello" in
  let result = Unicode_string.insert_string t ~pos:5 " world" in
  match result with
  | Ok t' -> Alcotest.(check string) "insert end" "hello world" (Unicode_string.to_string t')
  | Error _ -> Alcotest.fail "insert should succeed"

let test_insert_multibyte () =
  let t = ok_us "ac" in
  let result = Unicode_string.insert_string t ~pos:1 "中" in
  match result with
  | Ok t' ->
    Alcotest.(check string) "insert multibyte" "a中c" (Unicode_string.to_string t');
    Alcotest.(check int) "insert multibyte length" 3 (Unicode_string.length t');
    Alcotest.(check int) "insert multibyte width" 4 (Unicode_string.display_width t')
  | Error _ -> Alcotest.fail "insert should succeed"

let test_insert_invalid () =
  let t = ok_us "hello" in
  let result = Unicode_string.insert_string t ~pos:2 "\xFF\xFE" in
  Alcotest.(check bool) "insert invalid is error" true (Result.is_error result)

(* ===== Convenience ===== *)

let test_append () =
  let a = ok_us "hello" in
  let b = ok_us " world" in
  let t = Unicode_string.append a b in
  Alcotest.(check string) "append" "hello world" (Unicode_string.to_string t)

let test_concat () =
  let parts = List.map ok_us ["a"; "b"; "c"] in
  let t = Unicode_string.concat parts in
  Alcotest.(check string) "concat" "abc" (Unicode_string.to_string t)

let test_is_empty () =
  Alcotest.(check bool) "empty is_empty" true (Unicode_string.is_empty Unicode_string.empty);
  Alcotest.(check bool) "non-empty is_empty" false (Unicode_string.is_empty (ok_us "a"))

(* ===== Edge cases ===== *)

let test_delete_at_bounds () =
  let t = ok_us "a" in
  let t' = Unicode_string.delete t 0 in
  Alcotest.(check bool) "delete last char" true (Unicode_string.is_empty t')

let test_sub_empty_range () =
  let t = ok_us "hello" in
  let t' = Unicode_string.sub t ~start:2 ~len:0 in
  Alcotest.(check bool) "sub empty range" true (Unicode_string.is_empty t')

let test_insert_into_empty () =
  let t = Unicode_string.empty in
  let result = Unicode_string.insert_string t ~pos:0 "hello" in
  match result with
  | Ok t' -> Alcotest.(check string) "insert into empty" "hello" (Unicode_string.to_string t')
  | Error _ -> Alcotest.fail "insert should succeed"

(* ===== Width functions ===== *)

let test_prefix_width_ascii () =
  let t = ok_us "hello" in
  Alcotest.(check int) "prefix 0" 0 (Unicode_string.prefix_width t 0);
  Alcotest.(check int) "prefix 3" 3 (Unicode_string.prefix_width t 3);
  Alcotest.(check int) "prefix 5" 5 (Unicode_string.prefix_width t 5)

let test_prefix_width_wide () =
  let t = ok_us "a中b" in  (* widths: 1, 2, 1 *)
  Alcotest.(check int) "prefix 0" 0 (Unicode_string.prefix_width t 0);
  Alcotest.(check int) "prefix 1" 1 (Unicode_string.prefix_width t 1);
  Alcotest.(check int) "prefix 2" 3 (Unicode_string.prefix_width t 2);
  Alcotest.(check int) "prefix 3" 4 (Unicode_string.prefix_width t 3)

let test_grapheme_at_width_ascii () =
  let t = ok_us "hello" in
  Alcotest.(check int) "at width 0" 0 (Unicode_string.grapheme_at_width t 0);
  Alcotest.(check int) "at width 2" 2 (Unicode_string.grapheme_at_width t 2);
  Alcotest.(check int) "at width 5" 5 (Unicode_string.grapheme_at_width t 5)

let test_grapheme_at_width_wide () =
  let t = ok_us "a中b" in  (* widths: 1, 2, 1 = positions at 0, 1, 3 *)
  Alcotest.(check int) "at width 0" 0 (Unicode_string.grapheme_at_width t 0);
  Alcotest.(check int) "at width 1" 1 (Unicode_string.grapheme_at_width t 1);
  Alcotest.(check int) "at width 2" 1 (Unicode_string.grapheme_at_width t 2);  (* still on 中 *)
  Alcotest.(check int) "at width 3" 2 (Unicode_string.grapheme_at_width t 3)

(* ===== Split ===== *)

let test_split_middle () =
  let t = ok_us "hello" in
  let before, after = Unicode_string.split t 2 in
  Alcotest.(check string) "before" "he" (Unicode_string.to_string before);
  Alcotest.(check string) "after" "llo" (Unicode_string.to_string after)

let test_split_start () =
  let t = ok_us "hello" in
  let before, after = Unicode_string.split t 0 in
  Alcotest.(check bool) "before empty" true (Unicode_string.is_empty before);
  Alcotest.(check string) "after" "hello" (Unicode_string.to_string after)

let test_split_end () =
  let t = ok_us "hello" in
  let before, after = Unicode_string.split t 5 in
  Alcotest.(check string) "before" "hello" (Unicode_string.to_string before);
  Alcotest.(check bool) "after empty" true (Unicode_string.is_empty after)

let test_split_multibyte () =
  let t = ok_us "中文字" in
  let before, after = Unicode_string.split t 1 in
  Alcotest.(check string) "before" "中" (Unicode_string.to_string before);
  Alcotest.(check string) "after" "文字" (Unicode_string.to_string after)

(* ===== Wrap ===== *)

let test_wrap_no_wrap_needed () =
  let t = ok_us "hi" in
  let wrapped = Unicode_string.wrap t ~width:10 in
  Alcotest.(check int) "one line" 1 (List.length wrapped);
  Alcotest.(check string) "content" "hi" (Unicode_string.to_string (List.hd wrapped))

let test_wrap_exact () =
  let t = ok_us "hello" in
  let wrapped = Unicode_string.wrap t ~width:5 in
  (* Exact fit adds empty for cursor positioning *)
  Alcotest.(check int) "two entries" 2 (List.length wrapped);
  Alcotest.(check string) "content" "hello" (Unicode_string.to_string (List.hd wrapped));
  Alcotest.(check bool) "trailing empty" true (Unicode_string.is_empty (List.nth wrapped 1))

let test_wrap_overflow () =
  let t = ok_us "helloworld" in
  let wrapped = Unicode_string.wrap t ~width:5 in
  (* 10 chars = 2 full rows, adds empty for cursor *)
  Alcotest.(check int) "three entries" 3 (List.length wrapped);
  Alcotest.(check string) "line 1" "hello" (Unicode_string.to_string (List.nth wrapped 0));
  Alcotest.(check string) "line 2" "world" (Unicode_string.to_string (List.nth wrapped 1));
  Alcotest.(check bool) "trailing empty" true (Unicode_string.is_empty (List.nth wrapped 2))

let test_wrap_wide_chars () =
  let t = ok_us "a中b文c" in  (* widths: 1+2+1+2+1 = 7 *)
  let wrapped = Unicode_string.wrap t ~width:4 in
  (* a(1) + 中(2) + b(1) = 4 fits, 文(2) + c(1) = 3 fits *)
  (* Total width 7, 7 mod 4 != 0, so no trailing empty *)
  Alcotest.(check int) "two lines" 2 (List.length wrapped);
  Alcotest.(check string) "line 1" "a中b" (Unicode_string.to_string (List.nth wrapped 0));
  Alcotest.(check string) "line 2" "文c" (Unicode_string.to_string (List.nth wrapped 1))

let () =
  let open Alcotest in
  run "Unicode_string"
    [
      ( "construction",
        [
          test_case "empty" `Quick test_empty;
          test_case "ascii" `Quick test_ascii;
          test_case "multibyte utf8" `Quick test_multibyte_utf8;
          test_case "wide chars" `Quick test_wide_chars;
          test_case "combining chars" `Quick test_combining_chars;
          test_case "emoji simple" `Quick test_emoji_simple;
          test_case "emoji zwj" `Quick test_emoji_zwj;
          test_case "flag emoji" `Quick test_flag_emoji;
          test_case "invalid utf8" `Quick test_invalid_utf8;
          test_case "leading combiner" `Quick test_leading_combiner;
        ] );
      ( "access",
        [
          test_case "cluster_at" `Quick test_cluster_at;
          test_case "width_at" `Quick test_width_at;
        ] );
      ( "modification",
        [
          test_case "delete ascii" `Quick test_delete_ascii;
          test_case "delete multibyte" `Quick test_delete_multibyte;
          test_case "delete combining" `Quick test_delete_combining;
          test_case "delete range" `Quick test_delete_range;
          test_case "sub" `Quick test_sub;
          test_case "sub multibyte" `Quick test_sub_multibyte;
          test_case "insert middle" `Quick test_insert_string_middle;
          test_case "insert start" `Quick test_insert_string_start;
          test_case "insert end" `Quick test_insert_string_end;
          test_case "insert multibyte" `Quick test_insert_multibyte;
          test_case "insert invalid" `Quick test_insert_invalid;
        ] );
      ( "convenience",
        [
          test_case "append" `Quick test_append;
          test_case "concat" `Quick test_concat;
          test_case "is_empty" `Quick test_is_empty;
        ] );
      ( "edge_cases",
        [
          test_case "delete at bounds" `Quick test_delete_at_bounds;
          test_case "sub empty range" `Quick test_sub_empty_range;
          test_case "insert into empty" `Quick test_insert_into_empty;
        ] );
      ( "width_functions",
        [
          test_case "prefix_width ascii" `Quick test_prefix_width_ascii;
          test_case "prefix_width wide" `Quick test_prefix_width_wide;
          test_case "grapheme_at_width ascii" `Quick test_grapheme_at_width_ascii;
          test_case "grapheme_at_width wide" `Quick test_grapheme_at_width_wide;
        ] );
      ( "split",
        [
          test_case "split middle" `Quick test_split_middle;
          test_case "split start" `Quick test_split_start;
          test_case "split end" `Quick test_split_end;
          test_case "split multibyte" `Quick test_split_multibyte;
        ] );
      ( "wrap",
        [
          test_case "no wrap needed" `Quick test_wrap_no_wrap_needed;
          test_case "exact fit" `Quick test_wrap_exact;
          test_case "overflow" `Quick test_wrap_overflow;
          test_case "wide chars" `Quick test_wrap_wide_chars;
        ] );
    ]
