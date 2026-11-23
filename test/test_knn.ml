open OUnit2
open Quicksketch

(* Create a labeled drawing for testing. *)
let sample label features : Knn.sample = { label; features }

(* Unwrap a successful result or fail the test. *)
let get_ok = function
  | Ok value -> value
  | Error err -> assert_failure (Knn.error_to_string err)

(* Check that the expected error was returned. *)
let expect_error expected = function
  | Error actual -> assert_equal expected actual
  | Ok _ -> assert_failure "Expected an error"

(* Compare floating-point values approximately. *)
let approx expected actual =
  assert_bool "Floating-point values differ"
    (abs_float (expected -. actual) < 0.000001)

(* Extract category names from predictions. *)
let labels (predictions : Knn.prediction list) =
  List.map (fun (p : Knn.prediction) -> p.label) predictions

(* Get the highest-ranked category. *)
let top_label predictions =
  match labels predictions with
  | first :: _ -> first
  | [] -> assert_failure "No predictions returned"

(* Test 1: Distance to the same vector is zero. *)
let test_zero_distance _ =
  let d = get_ok (Knn.squared_distance [| 1.; 2. |] [| 1.; 2. |]) in
  approx 0. d

(* Test 2: 3 squared + 4 squared = 25. *)
let test_distance _ =
  let d = get_ok (Knn.squared_distance [| 0.; 0. |] [| 3.; 4. |]) in
  approx 25. d

(* Test 3: Distance is symmetric. *)
let test_symmetry _ =
  let a = [| 1.; 4. |] in
  let b = [| 3.; 2. |] in
  let ab = get_ok (Knn.squared_distance a b) in
  let ba = get_ok (Knn.squared_distance b a) in
  approx ab ba

(* Test 4: k=1 selects the closest drawing. *)
let test_one_neighbor _ =
  let training = [ sample "cat" [| 0.; 0. |]; sample "dog" [| 5.; 5. |] ] in

  let result = get_ok (Knn.predict ~k:1 ~training [| 0.1; 0. |]) in

  assert_equal "cat" (top_label result)

(* Test 5: Majority voting beats the closest individual. *)
let test_majority _ =
  let training =
    [ sample "cat" [| 0. |]; sample "cat" [| 0.2 |]; sample "dog" [| 0.1 |] ]
  in

  let result = get_ok (Knn.predict ~k:3 ~training [| 0.1 |]) in

  assert_equal "cat" (top_label result);
  assert_equal 2 (List.hd result).votes;
  approx (2. /. 3.) (List.hd result).vote_fraction

(* Test 6: Categories are returned in ranked order. *)
let test_ranked_guesses _ =
  let training =
    [ sample "cat" [| 0. |]; sample "tree" [| 1. |]; sample "house" [| 2. |] ]
  in

  let result = get_ok (Knn.predict ~k:3 ~training [| 0.9 |]) in

  assert_equal [ "tree"; "cat"; "house" ] (labels result)

(* Test 7: Equal votes favor the closer category. *)
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

(* Test 8: Exact ties use alphabetical order. *)
let test_alphabetical_tie _ =
  let training = [ sample "zebra" [| 0. |]; sample "apple" [| 2. |] ] in

  let result = get_ok (Knn.predict ~k:2 ~training [| 1. |]) in

  assert_equal "apple" (top_label result)

(* Test 9: k larger than the dataset uses all examples. *)
let test_large_k _ =
  let training = [ sample "cat" [| 0. |]; sample "cat" [| 0.1 |] ] in

  let result = get_ok (Knn.predict ~k:10 ~training [| 0. |]) in

  assert_equal 2 (List.hd result).votes

(* Test 10: k must be positive. *)
let test_invalid_k _ =
  let training = [ sample "cat" [| 0. |] ] in

  expect_error (Knn.Invalid_k 0) (Knn.predict ~k:0 ~training [| 0. |])

(* Test 11: Training set cannot be empty. *)
let test_empty_training _ =
  expect_error Knn.Empty_training_set (Knn.predict ~k:1 ~training:[] [| 0. |])

(* Test 12: Feature dimensions must match. *)
let test_dimension_mismatch _ =
  let training = [ sample "cat" [| 0.; 1. |] ] in

  expect_error
    (Knn.Dimension_mismatch { expected = 1; actual = 2 })
    (Knn.predict ~k:1 ~training [| 0. |])

(* Test 13: Every example needs a label. *)
let test_empty_label _ =
  let training = [ sample "" [| 0. |] ] in

  expect_error Knn.Empty_label (Knn.predict ~k:1 ~training [| 0. |])

(* Test 14: Infinity is not a valid feature. *)
let test_invalid_number _ =
  let training = [ sample "cat" [| infinity |] ] in

  expect_error Knn.Non_finite_value (Knn.predict ~k:1 ~training [| 0. |])

(* Test 15: The classifier must not mutate its inputs. *)
let test_input_unchanged _ =
  let features = [| 0.; 1. |] in
  let query = [| 0.5; 0.5 |] in
  let training = [ sample "cat" features ] in

  ignore (get_ok (Knn.predict ~k:1 ~training query));

  assert_equal [| 0.; 1. |] features;
  assert_equal [| 0.5; 0.5 |] query

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

let () = run_test_tt_main suite
