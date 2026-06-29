module M = Raoui.Md_layout

(* A UTF-8-aware display width: counts scalar values (bytes that are not
   continuation bytes 0b10xxxxxx). Good enough for the ASCII + bullet glyphs in
   these tests; production passes Unicode_string.display_width. *)
let measure s =
  let n = ref 0 in
  String.iter (fun c -> if Char.code c land 0xc0 <> 0x80 then incr n) s;
  !n

(* Concatenate the visible text of one laid-out line. *)
let text_of_line (line : M.line) =
  String.concat "" (List.map (fun (_, t) -> t) line)

(* Flatten blocks to a list of strings: prose lines verbatim, code blocks as
   "```<info>\n<code>" so structure is visible in assertions. *)
let render_to_lines md ~width =
  M.render ~width ~measure md
  |> List.concat_map (function
       | M.Lines ls -> List.map text_of_line ls
       | M.Code { info; code; _ } -> [ Printf.sprintf "```%s|%s" info code ])

let check_lines name ~width md expected =
  Alcotest.(check (list string)) name expected (render_to_lines md ~width)

let test_heading () =
  check_lines "heading" ~width:40 "# Hello world" [ "Hello world" ]

let test_paragraph_wrap () =
  (* width 10: "one two three four" wraps greedily *)
  check_lines "wrap" ~width:10 "one two three four"
    [ "one two"; "three four" ]

let test_bullet_list () =
  check_lines "bullets" ~width:40 "- alpha\n- beta"
    [ "\xe2\x80\xa2 alpha"; "\xe2\x80\xa2 beta" ]

let test_ordered_list () =
  check_lines "ordered" ~width:40 "1. first\n2. second"
    [ "1. first"; "2. second" ]

let test_code_block () =
  check_lines "code" ~width:40 "```r\nx <- 1\n```" [ "```r|x <- 1" ]

let test_blockquote () =
  check_lines "quote" ~width:40 "> quoted" [ "\xe2\x94\x82 quoted" ]

(* Emphasis must stack: bold inside a heading yields role=Heading + bold. *)
let test_emphasis_stacks () =
  let blocks = M.render ~width:40 ~measure:String.length "# a **b** c" in
  let spans =
    List.concat_map (function M.Lines ls -> List.concat ls | M.Code _ -> []) blocks
  in
  let bold_b =
    List.exists
      (fun ((s : M.style), t) ->
        t = "b" && s.emphasis.bold
        && match s.role with M.Heading 1 -> true | _ -> false)
      spans
  in
  Alcotest.(check bool) "bold-in-heading" true bold_b

let test_list_wrap_hanging_indent () =
  (* a long bullet wraps with continuation aligned under the text *)
  let lines = render_to_lines "- one two three four five" ~width:12 in
  Alcotest.(check (list string)) "hanging indent" [ "\xe2\x80\xa2 one two"; "  three four"; "  five" ] lines

let test_table_basic () =
  (* columns sized to widest cell; header rule of dashes; 2-space gaps *)
  let md = "| Name | Value |\n|------|-------|\n| alpha | 1.5 |\n| beta | 22.0 |" in
  check_lines "table" ~width:40 md
    [ "Name   Value"; "-----  -----"; "alpha  1.5"; "beta   22.0" ]

let test_table_right_align () =
  (* right-aligned column pads on the left *)
  let md = "| K | N |\n|:--|--:|\n| a | 1 |\n| bb | 200 |" in
  check_lines "right align" ~width:40 md
    [ "K     N"; "--  ---"; "a     1"; "bb  200" ]

let test_table_wraps_when_over_budget () =
  (* narrow width forces the wide column to wrap; the row gains height *)
  let md = "| id | note |\n|----|------|\n| 1 | one two three four |" in
  let lines = render_to_lines md ~width:14 in
  Alcotest.(check bool) "row wrapped to >1 data line" true (List.length lines > 3)

(* Integration: md_tty -> Terminal_ops ANSI. Confirms resolved faces emit the
   expected SGR introducers (bold=1, italic=3, underline=4). *)
let render_ansi md =
  Raoui.Md_tty.render ~width:40 Raoui.Theme.default md
  |> List.map (Raoui.Terminal_ops.Ansi.render_spans Raoui.Theme.default)
  |> String.concat "\n"

let contains haystack needle =
  let nl = String.length needle and hl = String.length haystack in
  let rec go i = i + nl <= hl && (String.sub haystack i nl = needle || go (i + 1)) in
  nl = 0 || go 0

let test_heading_bold () =
  Alcotest.(check bool) "heading bold SGR" true
    (contains (render_ansi "# Hi") "\x1b[0;1;")

let test_emphasis_italic () =
  Alcotest.(check bool) "italic SGR" true
    (contains (render_ansi "*x*") "\x1b[0;3;")

let test_link_underline () =
  Alcotest.(check bool) "link underline SGR" true
    (contains (render_ansi "[text](http://x)") "\x1b[0;4;")

(* Strikethrough (and any attribute) must not leak past its span: the plain
   word after ~~struck~~ must be emitted with a leading reset, not still
   carrying strike (9). *)
let test_strike_does_not_leak () =
  Alcotest.(check bool) "plain after strike is reset" true
    (contains (render_ansi "~~a~~ b") "\x1b[0;39;49mb")

let () =
  Alcotest.run "md_layout"
    [
      ( "blocks",
        [
          Alcotest.test_case "heading" `Quick test_heading;
          Alcotest.test_case "paragraph wrap" `Quick test_paragraph_wrap;
          Alcotest.test_case "bullet list" `Quick test_bullet_list;
          Alcotest.test_case "ordered list" `Quick test_ordered_list;
          Alcotest.test_case "code block" `Quick test_code_block;
          Alcotest.test_case "blockquote" `Quick test_blockquote;
          Alcotest.test_case "emphasis stacks" `Quick test_emphasis_stacks;
          Alcotest.test_case "list hanging indent" `Quick test_list_wrap_hanging_indent;
          Alcotest.test_case "table basic" `Quick test_table_basic;
          Alcotest.test_case "table right align" `Quick test_table_right_align;
          Alcotest.test_case "table wraps over budget" `Quick test_table_wraps_when_over_budget;
        ] );
      ( "ansi",
        [
          Alcotest.test_case "heading bold" `Quick test_heading_bold;
          Alcotest.test_case "emphasis italic" `Quick test_emphasis_italic;
          Alcotest.test_case "link underline" `Quick test_link_underline;
          Alcotest.test_case "strike no leak" `Quick test_strike_does_not_leak;
        ] );
    ]
