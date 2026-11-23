open OUnit2
open CS3110FinalProject

let pp_strokes ss =
  ss
  |> List.map (fun s ->
      s
      |> List.map (fun (x, y) -> Printf.sprintf "(%d,%d)" x y)
      |> String.concat " ")
  |> String.concat " | " |> Printf.sprintf "[%s]"

let assert_strokes expected d =
  assert_equal ~printer:pp_strokes expected (Drawing.strokes d)

let pp_bbox = function
  | None -> "None"
  | Some ((a, b), (c, d)) -> Printf.sprintf "Some ((%d,%d),(%d,%d))" a b c d

(* Simulate press -> drags -> release for one stroke. *)
let draw_stroke pts d =
  match pts with
  | [] -> d
  | p :: rest ->
      Drawing.start_stroke p d |> fun d ->
      List.fold_left (fun d q -> Drawing.add_point q d) d rest
      |> Drawing.end_stroke

let empty_tests =
  [
    ("empty has no strokes" >:: fun _ -> assert_strokes [] Drawing.empty);
    ( "empty is empty" >:: fun _ ->
      assert_bool "" (Drawing.is_empty Drawing.empty) );
    ( "empty not in stroke" >:: fun _ ->
      assert_bool "" (not (Drawing.in_stroke Drawing.empty)) );
    ( "empty bounding box is None" >:: fun _ ->
      assert_equal ~printer:pp_bbox None (Drawing.bounding_box Drawing.empty) );
  ]

let stroke_tests =
  [
    ( "one stroke keeps points in order" >:: fun _ ->
      draw_stroke [ (10, 20); (11, 22); (15, 30) ] Drawing.empty
      |> assert_strokes [ [ (10, 20); (11, 22); (15, 30) ] ] );
    ( "two strokes stay separate, oldest first" >:: fun _ ->
      Drawing.empty
      |> draw_stroke [ (1, 1); (2, 2) ]
      |> draw_stroke [ (50, 50); (60, 60) ]
      |> assert_strokes [ [ (1, 1); (2, 2) ]; [ (50, 50); (60, 60) ] ] );
    ( "single click is a one-point stroke" >:: fun _ ->
      draw_stroke [ (5, 5) ] Drawing.empty |> assert_strokes [ [ (5, 5) ] ] );
    ( "stroke in progress is visible and last" >:: fun _ ->
      let d =
        Drawing.empty
        |> draw_stroke [ (1, 1) ]
        |> Drawing.start_stroke (7, 7)
        |> Drawing.add_point (8, 8)
      in
      assert_bool "in stroke" (Drawing.in_stroke d);
      assert_strokes [ [ (1, 1) ]; [ (7, 7); (8, 8) ] ] d );
    ( "add_point with no stroke in progress is ignored" >:: fun _ ->
      Drawing.add_point (3, 3) Drawing.empty |> assert_strokes [] );
    ( "end_stroke with no stroke in progress is ignored" >:: fun _ ->
      let d = draw_stroke [ (1, 1) ] Drawing.empty in
      assert_strokes [ [ (1, 1) ] ] (Drawing.end_stroke d) );
    ( "start_stroke finishes a stroke left in progress" >:: fun _ ->
      Drawing.empty
      |> Drawing.start_stroke (1, 1)
      |> Drawing.add_point (2, 2)
      |> Drawing.start_stroke (9, 9)
      |> assert_strokes [ [ (1, 1); (2, 2) ]; [ (9, 9) ] ] );
    ( "duplicate consecutive points are dropped" >:: fun _ ->
      draw_stroke [ (4, 4); (4, 4); (4, 4); (5, 5); (5, 5) ] Drawing.empty
      |> assert_strokes [ [ (4, 4); (5, 5) ] ] );
    ( "non-consecutive repeats are kept" >:: fun _ ->
      draw_stroke [ (4, 4); (5, 5); (4, 4) ] Drawing.empty
      |> assert_strokes [ [ (4, 4); (5, 5); (4, 4) ] ] );
    ( "clear removes everything" >:: fun _ ->
      let d = Drawing.clear (draw_stroke [ (1, 1); (2, 2) ] Drawing.empty) in
      assert_bool "empty" (Drawing.is_empty d);
      assert_strokes [] d );
  ]

let clamp_tests =
  (* width = height = 280, so valid coordinates are 0..279. *)
  [
    ( "clamp leaves inside points alone" >:: fun _ ->
      assert_equal (100, 200) (Drawing.clamp (100, 200)) );
    ( "clamp negative to 0" >:: fun _ ->
      assert_equal (0, 0) (Drawing.clamp (-5, -1)) );
    ( "clamp too large to 279" >:: fun _ ->
      assert_equal (279, 279) (Drawing.clamp (280, 1000)) );
    ( "clamp corner values unchanged" >:: fun _ ->
      assert_equal (279, 0) (Drawing.clamp (279, 0)) );
    ( "out-of-bounds drag is clamped into the stroke" >:: fun _ ->
      draw_stroke [ (270, 10); (300, -20) ] Drawing.empty
      |> assert_strokes [ [ (270, 10); (279, 0) ] ] );
    ( "points that clamp to the same spot are merged" >:: fun _ ->
      draw_stroke [ (279, 5); (290, 5); (400, 5) ] Drawing.empty
      |> assert_strokes [ [ (279, 5) ] ] );
  ]

let bbox_tests =
  [
    ( "bounding box across two strokes" >:: fun _ ->
      (* min x 50, min y 40, max x 120, max y 150 *)
      Drawing.of_strokes [ [ (50, 80); (120, 40) ]; [ (90, 150) ] ]
      |> Drawing.bounding_box
      |> assert_equal ~printer:pp_bbox (Some ((50, 40), (120, 150))) );
    ( "bounding box of a single point" >:: fun _ ->
      Drawing.of_strokes [ [ (7, 9) ] ]
      |> Drawing.bounding_box
      |> assert_equal ~printer:pp_bbox (Some ((7, 9), (7, 9))) );
    ( "bounding box includes stroke in progress" >:: fun _ ->
      Drawing.of_strokes [ [ (10, 10) ] ]
      |> Drawing.start_stroke (200, 3)
      |> Drawing.bounding_box
      |> assert_equal ~printer:pp_bbox (Some ((10, 3), (200, 10))) );
  ]

let of_strokes_tests =
  [
    ( "of_strokes drops empty strokes and clamps" >:: fun _ ->
      Drawing.of_strokes [ []; [ (-1, 300); (2, 2) ]; [] ]
      |> assert_strokes [ [ (0, 279); (2, 2) ] ] );
    ( "of_strokes has no stroke in progress" >:: fun _ ->
      assert_bool ""
        (not (Drawing.in_stroke (Drawing.of_strokes [ [ (1, 1) ] ]))) );
    ( "of_strokes [] is empty" >:: fun _ ->
      assert_bool "" (Drawing.is_empty (Drawing.of_strokes [ [] ])) );
  ]

let to_string_tests =
  [
    ( "to_string one line per stroke" >:: fun _ ->
      Drawing.of_strokes [ [ (10, 20); (11, 22) ]; [ (5, 5) ] ]
      |> Drawing.to_string
      |> assert_equal ~printer:Fun.id "(10,20) (11,22)\n(5,5)" );
  ]

let () =
  run_test_tt_main
    ("drawing"
    >::: empty_tests @ stroke_tests @ clamp_tests @ bbox_tests
         @ of_strokes_tests @ to_string_tests)
