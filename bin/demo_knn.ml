open Quicksketch

(* Artificial reference drawings, represented as vectors. *)
let training : Knn.sample list =
  [
    { label = "cat"; features = [| 1.; 0.; 0.; 1. |] };
    { label = "cat"; features = [| 0.9; 0.; 0.; 1. |] };
    { label = "tree"; features = [| 0.; 1.; 1.; 0. |] };
    { label = "house"; features = [| 1.; 1.; 0.; 0. |] };
  ]

let () =
  (* A new, unlabeled drawing. *)
  let query = [| 1.; 0.1; 0.; 1. |] in

  match Knn.predict ~k:3 ~training query with
  | Error err -> prerr_endline (Knn.error_to_string err)
  | Ok predictions ->
      print_endline "QuickSketch predictions:";

      List.iter
        (fun (p : Knn.prediction) ->
          Printf.printf "%s: %d votes, %.2f vote fraction, distance %.3f\n"
            p.label p.votes p.vote_fraction p.nearest_distance)
        predictions
