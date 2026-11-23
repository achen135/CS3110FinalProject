(** Baseline k-nearest-neighbors classifier for QuickSketch.

    Input drawings must already be represented as feature vectors. Image
    normalization is handled by another module. *)

(* One labeled drawing from the reference dataset. *)
type sample = { label : string; features : float array }

(* A category predicted by k-NN. *)
type prediction = {
  label : string;
  votes : int;
  vote_fraction : float;
  nearest_distance : float;
}

(* Errors that can occur during classification. *)
type error =
  | Invalid_k of int
  | Empty_training_set
  | Empty_vector
  | Empty_label
  | Dimension_mismatch of { expected : int; actual : int }
  | Non_finite_value

(* Convert an error into a readable message. *)
val error_to_string : error -> string

(* Return squared Euclidean distance between two vectors. *)
val squared_distance : float array -> float array -> (float, error) result

(* Return category predictions, ranked from best to worst. *)
val predict :
  k:int ->
  training:sample list ->
  float array ->
  (prediction list, error) result
