(* Manual check for the canvas: draw, then press Enter to print the strokes.
   Run with: dune exec bin/draw_demo.exe *)
open CS3110FinalProject

let () =
  Canvas.open_window ();
  match Canvas.get_drawing ~prompt:"Draw anything\n[Enter] submit  [c] clear  [q] quit" with
  | None ->
      Canvas.close_window ();
      print_endline "Quit without submitting."
  | Some d ->
      Canvas.show_result ~target:"anything" ~guesses:[ "demo" ] ~correct:true;
      Canvas.close_window ();
      Printf.printf "Strokes: %d\n%s\n" (List.length (Drawing.strokes d))
        (Drawing.to_string d);
      match Drawing.bounding_box d with
      | Some ((x0, y0), (x1, y1)) ->
          Printf.printf "Bounding box: (%d,%d) to (%d,%d)\n" x0 y0 x1 y1
      | None -> ()
