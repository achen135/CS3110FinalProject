(** Baseline k-nearest-neighbors classifier for QuickSketch.

    The caller supplies labeled training examples and a query as numerical
    feature vectors. Image loading, resizing, and normalization are outside this
    module. *)

type sample = { label : string; features : float array }
(** A labeled reference example. [features] is the numerical representation of a
    drawing, and [label] is its category. [predict] validates these fields when
    the example is used. *)

type prediction = {
  label : string;
  votes : int;
  vote_fraction : float;
  nearest_distance : float;
}
(** Voting information for one category among the selected neighbors. [votes] is
    the number of neighbors with this label; [vote_fraction] is [votes] divided
    by the number selected; [nearest_distance] is the Euclidean distance to the
    closest selected neighbor with this label. The vote fraction is not a
    probability. *)

(** Possible validation and numerical errors returned by the classifier. *)
type error =
  | Invalid_k of int
  | Empty_training_set
  | Empty_vector
  | Empty_label
  | Dimension_mismatch of { expected : int; actual : int }
  | Non_finite_value

val error_to_string : error -> string
(** [error_to_string err] is a human-readable description of [err]. *)

val squared_distance : float array -> float array -> (float, error) result
(** [squared_distance a b] is the sum of squared differences between
    corresponding elements of [a] and [b]. It is [Error] when either array is
    empty, contains a non-finite value, has a different length, or when
    computing the distance overflows to a non-finite result. *)

val predict :
  k:int ->
  training:sample list ->
  float array ->
  (prediction list, error) result
(** [predict ~k ~training query] classifies [query] using the [k] closest
    examples in [training], where distance is squared Euclidean distance. Each
    selected example contributes one vote to its category. The result is
    [Ok predictions], ranked by descending vote count, then ascending nearest
    supporting distance, then alphabetical label order.

    When [k] exceeds the number of examples in [training], all are selected. The
    returned list contains only categories represented among those selected
    neighbors. Neither the query nor training arrays are modified.

    Returns [Error] when [k] is nonpositive, [training] is empty, a vector is
    empty, a label is blank, vector lengths do not match, or a value or
    calculated distance is non-finite. *)
