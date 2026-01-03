(** Main REPL implementation using lambda-term. *)

open Lwt.Infix

module Backend = Subprocess_backend.Backend

(** Custom read-line class with R syntax highlighting and completion *)
class read_line ~term ~history ~backend ~prompt = object(self)
  inherit LTerm_read_line.read_line ~history ()
  inherit [Zed_string.t] LTerm_read_line.term term

  method! show_box = false

  method! prompt = prompt

  (* Syntax highlighting *)
  method! stylise _last =
    let code = Zed_string.to_utf8 (Zed_rope.to_string self#input_prev) ^
               Zed_string.to_utf8 (Zed_rope.to_string self#input_next) in
    let styled = Highlighter.highlight code in
    (styled, 0)

  (* Tab completion *)
  method! completion =
    let code = Zed_string.to_utf8 (Zed_rope.to_string self#input_prev) in
    (* Find the word being completed *)
    let rec find_word_start i =
      if i <= 0 then 0
      else
        let c = code.[i - 1] in
        if R_lexer.is_ident_char c || c = '$' || c = '@' then
          find_word_start (i - 1)
        else i
    in
    let word_start = find_word_start (String.length code) in
    let prefix = String.sub code word_start (String.length code - word_start) in
    Lwt.async (fun () ->
      Backend.complete backend ~prefix >>= fun completions ->
      let completions =
        completions
        |> List.map (fun s ->
            let zs = Zed_string.of_utf8 s in
            (zs, zs))
      in
      self#set_completion word_start completions;
      Lwt.return ()
    )
end

(** The main prompt *)
let prompt = LTerm_text.(eval [S "R> "])
let continue_prompt = LTerm_text.(eval [S "R+ "])

(** Main REPL loop *)
let rec repl_loop term history backend accumulated =
  let prompt_text = if accumulated = "" then prompt else continue_prompt in
  let prompt_signal = React.S.const prompt_text in
  Lwt.catch
    (fun () ->
      let rl = new read_line ~term ~history:(LTerm_history.contents history) ~backend ~prompt:prompt_signal in
      rl#run >|= fun line -> Some (Zed_string.to_utf8 line))
    (function
      | LTerm_read_line.Interrupt ->
        LTerm.fprintl term "Interrupted" >>= fun () ->
        Lwt.return (Some "")
      | End_of_file ->
        Lwt.return None
      | exn ->
        Lwt.fail exn)
  >>= function
  | None ->
    (* EOF - exit *)
    LTerm.fprintl term "Goodbye!" >>= fun () ->
    Backend.shutdown backend
  | Some "" when accumulated = "" ->
    (* Empty line at top level - just continue *)
    repl_loop term history backend ""
  | Some line ->
    let full_input = if accumulated = "" then line else accumulated ^ "\n" ^ line in
    Backend.is_complete backend full_input >>= fun complete ->
    if not complete then
      (* Expression incomplete - accumulate more input *)
      repl_loop term history backend full_input
    else begin
      (* Evaluate the complete expression *)
      LTerm_history.add history (Zed_string.of_utf8 full_input);
      Backend.eval backend full_input >>= fun result ->
      (match result with
       | R_backend.Output s when s <> "" ->
         LTerm.fprintl term s
       | R_backend.Error s ->
         LTerm.fprintls term (LTerm_text.(eval [B_fg LTerm_style.lred; S "Error: "; S s; E_fg]))
       | _ -> Lwt.return ())
      >>= fun () ->
      repl_loop term history backend ""
    end

let main () =
  LTerm_inputrc.load () >>= fun () ->
  Lwt.catch
    (fun () -> Lazy.force LTerm.stdout)
    (fun _ -> Lwt.fail_with "Could not open terminal")
  >>= fun term ->
  LTerm.fprintls term LTerm_text.(eval [
    B_bold true;
    S "rocaml";
    E_bold;
    S " - R console (OCaml edition)\n";
    S "Type R expressions, Ctrl+D to exit\n"
  ]) >>= fun () ->
  Backend.start () >>= fun backend ->
  let history = LTerm_history.create [] in
  repl_loop term history backend ""

let () = Lwt_main.run (main ())
