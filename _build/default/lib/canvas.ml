open Graphics

let scale = 2
let text_height = 50
let pen_width = 4 * scale

(* Size of the drawing area on screen, in window pixels. *)
let area_w = Drawing.width * scale
let area_h = Drawing.height * scale

let open_window () =
  open_graph (Printf.sprintf " %dx%d" area_w (area_h + text_height));
  set_window_title "Quickdraw";
  auto_synchronize false

let close_window () = try close_graph () with Graphic_failure _ -> ()

(* Drawing points are in canvas coordinates; multiply by [scale] to get window
   pixels. *)
let draw_stroke stroke =
  match List.map (fun (x, y) -> (x * scale, y * scale)) stroke with
  | [] -> ()
  | [ (x, y) ] -> fill_circle x y (pen_width / 2)
  | (x, y) :: rest ->
      moveto x y;
      List.iter (fun (x, y) -> lineto x y) rest

let render d =
  set_color white;
  fill_rect 0 0 area_w area_h;
  set_color black;
  set_line_width pen_width;
  List.iter draw_stroke (Drawing.strokes d);
  set_line_width 1;
  moveto 0 area_h;
  lineto area_w area_h;
  synchronize ()

let show_text s =
  set_color (rgb 230 230 230);
  fill_rect 0 (area_h + 1) area_w (text_height - 1);
  set_color black;
  let lines = String.split_on_char '\n' s in
  let _, line_h = text_size "Ag" in
  (* Stack lines from the top of the strip downward. *)
  List.iteri
    (fun i line ->
      moveto 8 (area_h + text_height - ((i + 1) * (line_h + 4)));
      draw_string line)
    lines;
  synchronize ()

(* [loop d down] runs the input loop from drawing [d]; [down] records whether
   the mouse button was held at the previous event, so we can tell a press and a
   release apart from a drag. *)
let rec loop d down =
  let st =
    wait_next_event [ Button_down; Button_up; Mouse_motion; Key_pressed ]
  in
  if st.keypressed then
    match st.key with
    | 'c' | 'C' ->
        render Drawing.empty;
        loop (Drawing.clear d) false
    | '\r' | '\n' ->
        if Drawing.is_empty d then loop d down else Some (Drawing.end_stroke d)
    | '\027' | 'q' | 'Q' -> None
    | _ -> loop d down
  else
    let p = (st.mouse_x / scale, st.mouse_y / scale) in
    let d', down' =
      match (st.button, down) with
      | true, false -> (Drawing.start_stroke p d, true)
      | true, true -> (Drawing.add_point p d, true)
      | false, true -> (Drawing.end_stroke d, false)
      | false, false -> (d, false)
    in
    if d' != d then render d';
    loop d' down'

let get_drawing ~prompt =
  show_text prompt;
  render Drawing.empty;
  try loop Drawing.empty false with Graphic_failure _ -> None

let show_result ~target ~guesses ~correct =
  let verdict = if correct then "Correct!" else "Not quite." in
  let guess_str =
    match guesses with
    | [] -> "(none)"
    | gs -> String.concat ", " gs
  in
  show_text
    (Printf.sprintf "Target: %s - %s\nGuesses: %s  (press a key)" target verdict
       guess_str);
  try ignore (wait_next_event [ Key_pressed ]) with Graphic_failure _ -> ()
