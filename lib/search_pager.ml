(* Packs history-search matches into a fixed budget of display rows.

   Every entry gets one row (its first line); the selected entry expands to
   show its full text — it is the preview of what Enter will insert — but
   always leaves room for up to [min_context] other entries so alternatives
   stay visible. The window over the match list slides so the selected
   entry's block is always fully on screen. *)

type row = {
  entry_index : int;  (* index into the matches list this row belongs to *)
  line : string;      (* one source line of the entry (untruncated) *)
  first : bool;       (* this is the entry's first visible row *)
  hidden_lines : int; (* source lines of this entry not shown; >0 only on its
                         last visible row *)
}

(* Other entries guaranteed visible alongside an expanded selection (fewer if
   there are fewer matches). *)
let min_context = 3

(* Budget of list rows below the query row: up to 10, capped so the query row
   and one row of breathing room still fit on a small terminal. *)
let max_rows ~term_height = min 10 (max 0 (term_height - 2))

let view_rows ~max_rows ~selected (matches : (string * string) list) =
  let n = List.length matches in
  if n = 0 || max_rows <= 0 then []
  else begin
    assert (selected >= 0 && selected < n);
    let lines_of i =
      let _mode, text = List.nth matches i in
      String.split_on_char '\n' text
    in
    let selected_lines = lines_of selected in
    let selected_height =
      min
        (List.length selected_lines)
        (max 1 (max_rows - min min_context (n - 1)))
    in
    (* Slide the window down just enough that the rows before the selected
       entry (one each) plus its expanded block fit in the budget. *)
    let window_start = max 0 (selected - (max_rows - selected_height)) in
    let rows = ref [] in
    let budget = ref max_rows in
    let i = ref window_start in
    while !budget > 0 && !i < n do
      let entry_lines = lines_of !i in
      let total = List.length entry_lines in
      let height =
        if !i = selected then min selected_height !budget else 1
      in
      List.filteri (fun j _ -> j < height) entry_lines
      |> List.iteri (fun j line ->
             rows :=
               {
                 entry_index = !i;
                 line;
                 first = j = 0;
                 hidden_lines = (if j = height - 1 then total - height else 0);
               }
               :: !rows);
      budget := !budget - height;
      incr i
    done;
    List.rev !rows
  end
