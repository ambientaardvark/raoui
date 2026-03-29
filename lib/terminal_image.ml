type rendered = {
  output : string;
  rows : int;
}

let base64_table =
  "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/"

let base64_encode s =
  let len = String.length s in
  let out_len = ((len + 2) / 3) * 4 in
  let out = Bytes.create out_len in
  let rec loop src_idx dst_idx =
    if src_idx >= len then ()
    else
      let b0 = Char.code s.[src_idx] in
      let has_b1 = src_idx + 1 < len in
      let has_b2 = src_idx + 2 < len in
      let b1 = if has_b1 then Char.code s.[src_idx + 1] else 0 in
      let b2 = if has_b2 then Char.code s.[src_idx + 2] else 0 in
      let triple = (b0 lsl 16) lor (b1 lsl 8) lor b2 in
      Bytes.set out dst_idx base64_table.[(triple lsr 18) land 0x3f];
      Bytes.set out (dst_idx + 1) base64_table.[(triple lsr 12) land 0x3f];
      Bytes.set out (dst_idx + 2)
        (if has_b1 then base64_table.[(triple lsr 6) land 0x3f] else '=');
      Bytes.set out (dst_idx + 3)
        (if has_b2 then base64_table.[triple land 0x3f] else '=');
      loop (src_idx + 3) (dst_idx + 4)
  in
  loop 0 0;
  Bytes.unsafe_to_string out

let read_file path =
  let ic = open_in_bin path in
  Fun.protect
    (fun () ->
      let len = in_channel_length ic in
      really_input_string ic len)
    ~finally:(fun () -> close_in_noerr ic)

let clamp min_value max_value value =
  max min_value (min max_value value)

let estimate_rows ~width_px ~height_px cols =
  let cell_aspect = 0.5 in
  let rows_f =
    (float_of_int height_px /. float_of_int width_px)
    *. float_of_int cols *. cell_aspect
  in
  max 1 (int_of_float (ceil rows_f))

let choose_cols ~term_width ~max_width_cols ~max_height_rows ~width_px ~height_px
    =
  let width_cap = max 1 (min max_width_cols (max 1 (term_width - 1))) in
  let height_limited =
    int_of_float
      (floor
         ((float_of_int max_height_rows *. float_of_int width_px)
         /. (float_of_int height_px *. 0.5)))
  in
  clamp 1 width_cap (min width_cap (max 1 height_limited))

let kitty_chunk ?payload params =
  let body =
    match payload with
    | Some data -> params ^ ";" ^ data
    | None -> params
  in
  "\x1b_G" ^ body ^ "\x1b\\"

let render_kitty ~config ~term_width ~image =
  match (image.Ffi_backend.width_px, image.Ffi_backend.height_px) with
  | Some width_px, Some height_px when width_px > 0 && height_px > 0 -> (
      try
        let cols =
          choose_cols ~term_width
            ~max_width_cols:config.User_options.inline_image_max_width_cols
            ~max_height_rows:config.User_options.inline_image_max_height_rows
            ~width_px ~height_px
        in
        let rows = estimate_rows ~width_px ~height_px cols in
        let rows =
          min rows config.User_options.inline_image_max_height_rows
        in
        let encoded = read_file image.preview_path |> base64_encode in
        let encoded_chunk_size = 4096 in
        let total_len = String.length encoded in
        let buf = Buffer.create (total_len + 256) in
        let rec add_chunks offset first =
          if offset < total_len then
            let chunk_len = min encoded_chunk_size (total_len - offset) in
            let payload = String.sub encoded offset chunk_len in
            let has_more = offset + chunk_len < total_len in
            let params =
              if first then
                Printf.sprintf "a=T,f=100,t=d,c=%d,r=%d,m=%d,q=2"
                  cols
                  rows
                  (if has_more then 1 else 0)
              else
                Printf.sprintf "m=%d" (if has_more then 1 else 0)
            in
            Buffer.add_string buf (kitty_chunk ~payload params);
            add_chunks (offset + chunk_len) false
        in
        add_chunks 0 true;
        Buffer.add_string buf "\r\n";
        Some { output = Buffer.contents buf; rows }
      with Sys_error _ -> None)
  | _ -> None

let render ~terminal_capabilities ~config ~term_width ~image =
  match terminal_capabilities.Terminal_capabilities.image_protocol with
  | Terminal_capabilities.Kitty -> render_kitty ~config ~term_width ~image
  | Terminal_capabilities.ITerm | Terminal_capabilities.No_image -> None
