(** Baseline k-NN implementation for QuickSketch. *)

type sample = { label : string; features : float array }

type prediction = {
  label : string;
  votes : int;
  vote_fraction : float;
  nearest_distance : float;
}

type error =
  | Invalid_k of int
  | Empty_training_set
  | Empty_vector
  | Empty_label
  | Dimension_mismatch of { expected : int; actual : int }
  | Non_finite_value

(* Translate errors into messages the game can display. *)
let error_to_string = function
  | Invalid_k k -> Printf.sprintf "k must be positive, received %d" k
  | Empty_training_set -> "The reference dataset is empty"
  | Empty_vector -> "Feature vectors cannot be empty"
  | Empty_label -> "Training examples must have labels"
  | Dimension_mismatch { expected; actual } ->
      Printf.sprintf "Expected %d features, received %d" expected actual
  | Non_finite_value -> "Feature vectors contain invalid numerical values"

(* Reject NaN and infinity, since they cannot be used
   reliably when comparing distances. *)
let is_finite x =
  match classify_float x with
  | FP_nan | FP_infinite -> false
  | FP_normal | FP_subnormal | FP_zero -> true

(* Check that a feature vector is valid. *)
let validate_vector vector =
  if Array.length vector = 0 then Error Empty_vector
  else if Array.for_all is_finite vector then Ok ()
  else Error Non_finite_value

(* Internal distance calculation.
   Assumes both vectors have already been validated. *)
let squared_distance_unchecked a b =
  let total = ref 0.0 in

  for i = 0 to Array.length a - 1 do
    let difference = a.(i) -. b.(i) in
    total := !total +. (difference *. difference)
  done;

  !total

(* Public, validated squared Euclidean distance. *)
let squared_distance a b =
  match validate_vector a with
  | Error err -> Error err
  | Ok () -> (
      match validate_vector b with
      | Error err -> Error err
      | Ok () ->
          if Array.length a <> Array.length b then
            Error
              (Dimension_mismatch
                 { expected = Array.length a; actual = Array.length b })
          else
            let distance = squared_distance_unchecked a b in
            if is_finite distance then Ok distance else Error Non_finite_value)

(* Check every labeled reference drawing. *)
let rec validate_samples expected_length (samples : sample list) =
  match samples with
  | [] -> Ok ()
  | current :: remaining -> (
      if String.trim current.label = "" then Error Empty_label
      else
        match validate_vector current.features with
        | Error err -> Error err
        | Ok () ->
            let actual = Array.length current.features in

            if actual <> expected_length then
              Error (Dimension_mismatch { expected = expected_length; actual })
            else validate_samples expected_length remaining)

(* Return the first n elements of a list. *)
let rec take n items =
  if n <= 0 then []
  else match items with [] -> [] | head :: tail -> head :: take (n - 1) tail

(* A map from category name to vote information. *)
module LabelMap = Map.Make (String)

(* Sort neighbors from smallest to largest distance. *)
let compare_neighbors (label_a, distance_a) (label_b, distance_b) =
  let by_distance = Float.compare distance_a distance_b in

  if by_distance <> 0 then by_distance else String.compare label_a label_b

(* Sort predicted categories:
   1. Most votes first
   2. Smaller nearest distance
   3. Alphabetical order *)
let compare_predictions first second =
  let by_votes = Int.compare second.votes first.votes in

  if by_votes <> 0 then by_votes
  else
    let by_distance =
      Float.compare first.nearest_distance second.nearest_distance
    in

    if by_distance <> 0 then by_distance
    else String.compare first.label second.label

(* Predict the categories of a drawing using k-NN. *)
let predict ~k ~training query =
  if k <= 0 then Error (Invalid_k k)
  else if training = [] then Error Empty_training_set
  else
    match validate_vector query with
    | Error err -> Error err
    | Ok () -> (
        match validate_samples (Array.length query) training with
        | Error err -> Error err
        | Ok () ->
            (* Step 1: Calculate distances to all examples. *)
            let neighbors =
              List.map
                (fun (example : sample) ->
                  ( example.label,
                    squared_distance_unchecked query example.features ))
                training
            in

            if List.exists (fun (_, d) -> not (is_finite d)) neighbors then
              Error Non_finite_value
            else
              (* Step 2: Select the k nearest examples. *)
              let count = min k (List.length training) in

              let nearest =
                neighbors |> List.sort compare_neighbors |> take count
              in

              (* Step 3: Count votes for each category. *)
              let votes =
                List.fold_left
                  (fun map (label, distance) ->
                    let new_count, nearest_distance =
                      match LabelMap.find_opt label map with
                      | None -> (1, distance)
                      | Some (old_count, old_distance) ->
                          (old_count + 1, min old_distance distance)
                    in

                    LabelMap.add label (new_count, nearest_distance) map)
                  LabelMap.empty nearest
              in

              (* Step 4: Build ranked predictions. *)
              let predictions =
                LabelMap.bindings votes
                |> List.map (fun (label, (num_votes, distance)) ->
                    {
                      label;
                      votes = num_votes;
                      vote_fraction =
                        float_of_int num_votes /. float_of_int count;
                      nearest_distance = sqrt distance;
                    })
                |> List.sort compare_predictions
              in

              Ok predictions)
