open Quicksketch

(* The five categories used in this demo. *)
let categories = [| "cat"; "tree"; "house"; "bicycle"; "airplane" |]

(* Find the index of a category in the array above. *)
let category_index label =
  let rec find i =
    if i = Array.length categories then failwith ("Unknown category: " ^ label)
    else if categories.(i) = label then i
    else find (i + 1)
  in
  find 0

(* Read the numerical drawings prepared by the Python script. *)
let load_data filename =
  let input = open_in filename in

  let training = ref [] in
  let testing = ref [] in

  (try
     while true do
       match String.split_on_char '\t' (input_line input) with
       | [ split; label; pixels ] ->
           let features =
             String.split_on_char ',' pixels
             |> List.map (fun value -> float_of_string value /. 255.)
             |> Array.of_list
           in

           if Array.length features <> 784 then failwith "Expected 784 pixels";

           let drawing : Knn.sample = { label; features } in

           if split = "train" then training := drawing :: !training
           else if split = "test" then testing := drawing :: !testing
           else failwith "Unknown data split"
       | _ -> failwith "Invalid data row"
     done
   with End_of_file -> ());

  close_in input;

  (List.rev !training, List.rev !testing)

(* Display a 28x28 drawing in the terminal. *)
let print_drawing (features : float array) =
  for row = 0 to 27 do
    for column = 0 to 27 do
      let pixel = features.((row * 28) + column) in
      print_string (if pixel > 0.3 then "##" else "  ")
    done;
    print_newline ()
  done

(* Evaluate k-NN on the unseen drawings. *)
let () =
  let filename = "data/quickdraw_demo.tsv" in

  if not (Sys.file_exists filename) then (
    prerr_endline "Run python3 scripts/prepare_quickdraw.py first";
    exit 1);

  let training, testing = load_data filename in

  Printf.printf "Loaded %d training and %d test drawings\n"
    (List.length training) (List.length testing);

  (* Count total and correct predictions for each category. *)
  let totals = Array.make 5 0 in
  let correct = Array.make 5 0 in

  List.iter
    (fun (drawing : Knn.sample) ->
      (* Use YOUR existing classifier! *)
      match Knn.predict ~k:3 ~training drawing.features with
      | Error error -> failwith (Knn.error_to_string error)
      | Ok [] -> failwith "No guess returned"
      | Ok ((best : Knn.prediction) :: _) ->
          let category = category_index drawing.label in

          totals.(category) <- totals.(category) + 1;

          if best.label = drawing.label then
            correct.(category) <- correct.(category) + 1;

          (* Display the first three test drawings. *)
          if totals.(category) = 1 then (
            Printf.printf "\nActual: %s\n" drawing.label;
            print_drawing drawing.features;

            Printf.printf "Predicted: %s (%s)\n" best.label
              (if best.label = drawing.label then "correct" else "wrong")))
    testing;

  (* Print classification accuracy for each category. *)
  print_endline "\n--- Accuracy on unseen drawings ---";

  for i = 0 to 4 do
    Printf.printf "%9s: %d/%d correct (%.1f%%)\n" categories.(i) correct.(i)
      totals.(i)
      (100. *. float_of_int correct.(i) /. float_of_int totals.(i))
  done;

  (* Calculate the overall recognition accuracy. *)
  let total = Array.fold_left ( + ) 0 totals in
  let hits = Array.fold_left ( + ) 0 correct in

  Printf.printf "OVERALL: %d/%d correct (%.1f%%)\n" hits total
    (100. *. float_of_int hits /. float_of_int total)
