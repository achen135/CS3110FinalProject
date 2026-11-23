(** Baseline k-NN implementation for QuickSketch. Private helper specifications
    describe implementation details; public behavior is specified in [Knn]'s
    interface. *)

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

(* Centralize the game-facing descriptions of each error constructor. *)
let error_to_string = function
  | Invalid_k k -> Printf.sprintf "k must be positive, received %d" k
  | Empty_training_set -> "The reference dataset is empty"
  | Empty_vector -> "Feature vectors cannot be empty"
  | Empty_label -> "Training examples must have labels"
  | Dimension_mismatch { expected; actual } ->
      Printf.sprintf "Expected %d features, received %d" expected actual
  | Non_finite_value -> "Feature vectors contain invalid numerical values"

(** [is_finite x] is [true] exactly when [x] is neither NaN nor infinite.
    Normal, subnormal, and zero floating-point values are accepted. *)
let is_finite x =
  match classify_float x with
  | FP_nan | FP_infinite -> false
  | FP_normal | FP_subnormal | FP_zero -> true

(** [validate_vector vector] is [Ok ()] if [vector] is nonempty and all of its
    values are finite. Otherwise, it is [Error Empty_vector] or
    [Error Non_finite_value]. It does not modify [vector]. *)
let validate_vector vector =
  if Array.length vector = 0 then Error Empty_vector
  else if Array.for_all is_finite vector then Ok ()
  else Error Non_finite_value

(** [squared_distance_unchecked a b] sums squared elementwise differences.
    Requires: [a] and [b] have the same length and contain finite values. Unlike
    [squared_distance], it does not validate either array. *)
let squared_distance_unchecked a b =
  let total = ref 0.0 in

  for i = 0 to Array.length a - 1 do
    let difference = a.(i) -. b.(i) in
    total := !total +. (difference *. difference)
  done;

  !total

(* Validate both arrays and the computed result before returning distance.
   The public contract, including error cases, is in [knn.mli]. *)
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

(** [validate_samples expected_length samples] is [Ok ()] if every sample has a
    nonblank label and a nonempty, finite feature vector of length
    [expected_length]. Otherwise, it returns the first validation error. An
    empty list is valid. *)
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

(** [take n items] is the prefix of [items] containing at most [n] elements. It
    is [[]] when [n <= 0], and all of [items] when [n] is at least the list
    length. *)
let rec take n items =
  if n <= 0 then []
  else match items with [] -> [] | head :: tail -> head :: take (n - 1) tail

(* Store each category's (vote count, smallest squared distance) pair. *)
module LabelMap = Map.Make (String)

(** [compare_neighbors a b] orders neighbors by increasing squared distance,
    breaking equal-distance ties alphabetically by label. Requires: both
    distances are finite. *)
let compare_neighbors (label_a, distance_a) (label_b, distance_b) =
  let by_distance = Float.compare distance_a distance_b in

  if by_distance <> 0 then by_distance else String.compare label_a label_b

(** [compare_predictions first second] orders prediction records by decreasing
    vote count, then increasing nearest supporting distance, then alphabetical
    label. A result of zero means equal ranking. *)
let compare_predictions first second =
  let by_votes = Int.compare second.votes first.votes in

  if by_votes <> 0 then by_votes
  else
    let by_distance =
      Float.compare first.nearest_distance second.nearest_distance
    in

    if by_distance <> 0 then by_distance
    else String.compare first.label second.label

(* Implementation of [predict] from [knn.mli]. The algorithm validates
   the inputs, calculates distances, selects neighbors, and tallies votes.
   The interface documents its return value and error conditions. *)
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
            (* Each neighbor stores its category and squared distance to the query. *)
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
              (* Sort the reference examples, then keep at most [k] neighbors. *)
              let count = min k (List.length training) in

              let nearest =
                neighbors |> List.sort compare_neighbors |> take count
              in

              (* Accumulate each label's vote count and closest squared distance. *)
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

              (* Convert squared distances to Euclidean distances for reporting,
                 and order the categories using [compare_predictions]. *)
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
