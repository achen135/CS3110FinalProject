open OUnit2
open Quicksketch

(** [sample label features] makes a labeled reference example with the specified
    category and features. *)
let sample label features : Knn.sample = { label; features }

(** [get_ok result] returns the value of [Ok value], or fails the OUnit test
    with the error description if [result] is [Error _]. *)
let get_ok = function
  | Ok value -> value
  | Error err -> assert_failure (Knn.error_to_string err)

(** [expect_error expected result] passes when [result] is [Error expected]. It
    fails on a different error or on [Ok _]. *)
let expect_error expected = function
  | Error actual -> assert_equal expected actual
  | Ok _ -> assert_failure "Expected an error"

(** [approx expected actual] checks that the absolute difference is less than
    [0.000001], failing the OUnit test otherwise. *)
let approx expected actual =
  assert_bool "Floating-point values differ"
    (abs_float (expected -. actual) < 0.000001)

(** [labels predictions] returns the prediction labels in their existing order.
*)
let labels (predictions : Knn.prediction list) =
  List.map (fun (p : Knn.prediction) -> p.label) predictions

(** [top_label predictions] is the first label, or fails the OUnit test if
    [predictions] is empty. Requires: the list is already ranked. *)
let top_label predictions =
  match labels predictions with
  | first :: _ -> first
  | [] -> assert_failure "No predictions returned"

(** [test_zero_distance _] verifies that the squared distance from a vector to
    itself is zero. *)
let test_zero_distance _ =
  let d = get_ok (Knn.squared_distance [| 1.; 2. |] [| 1.; 2. |]) in
  approx 0. d

(** [test_distance _] verifies a known squared distance: a 3-4 offset must
    produce 25. *)
let test_distance _ =
  let d = get_ok (Knn.squared_distance [| 0.; 0. |] [| 3.; 4. |]) in
  approx 25. d

(** [test_symmetry _] verifies that swapping two vectors leaves their squared
    distance unchanged. *)
let test_symmetry _ =
  let a = [| 1.; 4. |] in
  let b = [| 3.; 2. |] in
  let ab = get_ok (Knn.squared_distance a b) in
  let ba = get_ok (Knn.squared_distance b a) in
  approx ab ba

(** [test_one_neighbor _] verifies that with [k=1], the closest reference
    example determines the prediction. *)
let test_one_neighbor _ =
  let training = [ sample "cat" [| 0.; 0. |]; sample "dog" [| 5.; 5. |] ] in

  let result = get_ok (Knn.predict ~k:1 ~training [| 0.1; 0. |]) in

  assert_equal "cat" (top_label result)

(** [test_majority _] verifies that two cat votes defeat one dog vote, even
    though the dog is the closest single example. *)
let test_majority _ =
  let training =
    [ sample "cat" [| 0. |]; sample "cat" [| 0.2 |]; sample "dog" [| 0.1 |] ]
  in

  let result = get_ok (Knn.predict ~k:3 ~training [| 0.1 |]) in

  assert_equal "cat" (top_label result);
  assert_equal 2 (List.hd result).votes;
  approx (2. /. 3.) (List.hd result).vote_fraction

(** [test_ranked_guesses _] verifies prediction ordering when categories have
    equal votes but different nearest distances. *)
let test_ranked_guesses _ =
  let training =
    [ sample "cat" [| 0. |]; sample "tree" [| 1. |]; sample "house" [| 2. |] ]
  in

  let result = get_ok (Knn.predict ~k:3 ~training [| 0.9 |]) in

  assert_equal [ "tree"; "cat"; "house" ] (labels result)

(** [test_distance_tie _] verifies that tied vote counts are broken by the
    nearest supporting example. *)
let test_distance_tie _ =
  let training =
    [
      sample "cat" [| 0. |];
      sample "cat" [| 0.2 |];
      sample "dog" [| 0.3 |];
      sample "dog" [| 0.8 |];
    ]
  in

  let result = get_ok (Knn.predict ~k:4 ~training [| 0.45 |]) in

  assert_equal "dog" (top_label result)

(** [test_alphabetical_tie _] verifies alphabetical ordering when both votes and
    nearest supporting distances are tied. *)
let test_alphabetical_tie _ =
  let training = [ sample "zebra" [| 0. |]; sample "apple" [| 2. |] ] in

  let result = get_ok (Knn.predict ~k:2 ~training [| 1. |]) in

  assert_equal "apple" (top_label result)

(** [test_large_k _] verifies that requesting more neighbors than there are
    training examples selects all available examples. *)
let test_large_k _ =
  let training = [ sample "cat" [| 0. |]; sample "cat" [| 0.1 |] ] in

  let result = get_ok (Knn.predict ~k:10 ~training [| 0. |]) in

  assert_equal 2 (List.hd result).votes

(** [test_invalid_k _] verifies that [k=0] produces [Invalid_k]. *)
let test_invalid_k _ =
  let training = [ sample "cat" [| 0. |] ] in

  expect_error (Knn.Invalid_k 0) (Knn.predict ~k:0 ~training [| 0. |])

(** [test_empty_training _] verifies the [Empty_training_set] error. *)
let test_empty_training _ =
  expect_error Knn.Empty_training_set (Knn.predict ~k:1 ~training:[] [| 0. |])

(** [test_dimension_mismatch _] verifies that differently sized feature vectors
    return [Dimension_mismatch]. *)
let test_dimension_mismatch _ =
  let training = [ sample "cat" [| 0.; 1. |] ] in

  expect_error
    (Knn.Dimension_mismatch { expected = 1; actual = 2 })
    (Knn.predict ~k:1 ~training [| 0. |])

(** [test_empty_label _] verifies that a blank category returns [Empty_label].
*)
let test_empty_label _ =
  let training = [ sample "" [| 0. |] ] in

  expect_error Knn.Empty_label (Knn.predict ~k:1 ~training [| 0. |])

(** [test_invalid_number _] verifies that an infinite feature produces
    [Non_finite_value]. *)
let test_invalid_number _ =
  let training = [ sample "cat" [| infinity |] ] in

  expect_error Knn.Non_finite_value (Knn.predict ~k:1 ~training [| 0. |])

(** [test_input_unchanged _] verifies that classification preserves the original
    query and reference feature arrays. *)
let test_input_unchanged _ =
  let features = [| 0.; 1. |] in
  let query = [| 0.5; 0.5 |] in
  let training = [ sample "cat" features ] in

  ignore (get_ok (Knn.predict ~k:1 ~training query));

  assert_equal [| 0.; 1. |] features;
  assert_equal [| 0.5; 0.5 |] query

(* Group the 15 independent cases into one OUnit suite. *)
let suite =
  "QuickSketch k-NN"
  >::: [
         "zero distance" >:: test_zero_distance;
         "distance calculation" >:: test_distance;
         "distance symmetry" >:: test_symmetry;
         "one neighbor" >:: test_one_neighbor;
         "majority vote" >:: test_majority;
         "ranked guesses" >:: test_ranked_guesses;
         "distance tie" >:: test_distance_tie;
         "alphabetical tie" >:: test_alphabetical_tie;
         "large k" >:: test_large_k;
         "invalid k" >:: test_invalid_k;
         "empty training" >:: test_empty_training;
         "dimension mismatch" >:: test_dimension_mismatch;
         "empty label" >:: test_empty_label;
         "invalid number" >:: test_invalid_number;
         "input unchanged" >:: test_input_unchanged;
       ]

(* Run the suite and report any failures to the test runner. *)
let () = run_test_tt_main suite
