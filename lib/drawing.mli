(** A player's sketch, stored as a sequence of strokes.

    This module is pure: it does not depend on any graphics library, so it can
    be built and tested without opening a window. The canvas records mouse input
    into a [Drawing.t], and normalization converts a [Drawing.t] into a feature
    vector. *)

val width : int
(** Width of the drawing area in pixels (280, i.e. 10 px per cell of a 28x28
    grid). *)

val height : int
(** Height of the drawing area in pixels (280). *)

type point = int * int
(** A pixel position [(x, y)] on the drawing area. The origin [(0, 0)] is the
    bottom-left corner and [y] increases upward (the [graphics] library
    convention). Every point stored in a drawing satisfies [0 <= x < width] and
    [0 <= y < height]. *)

type stroke = point list
(** One continuous pen movement, from mouse press to mouse release. Points are
    in the order they were drawn. Consecutive points may be several pixels apart
    because the mouse is sampled sparsely, so consumers that rasterize a stroke
    should connect consecutive points with line segments. A stroke always has at
    least one point; a single point is a click. *)

type t
(** A drawing: zero or more strokes, possibly with one stroke still in progress
    (the mouse button is held down). Abstract, so a drawing can only be built
    through the operations below and always satisfies the invariants above. *)

val empty : t
(** The drawing with no strokes and no stroke in progress. *)

val clamp : point -> point
(** [clamp p] moves [p] to the nearest point inside the drawing area. Points
    already inside are unchanged. *)

val start_stroke : point -> t -> t
(** [start_stroke p d] begins a new stroke at [clamp p]. If a stroke was already
    in progress in [d], it is finished first. *)

val add_point : point -> t -> t
(** [add_point p d] appends [clamp p] to the stroke in progress. If no stroke is
    in progress, returns [d] unchanged. If [clamp p] equals the last point of
    the current stroke, returns [d] unchanged (no duplicate points). *)

val end_stroke : t -> t
(** [end_stroke d] finishes the stroke in progress. If no stroke is in progress,
    returns [d] unchanged. *)

val clear : t -> t
(** [clear d] is [empty]. Provided so callers express intent; the argument is
    ignored. *)

val of_strokes : stroke list -> t
(** [of_strokes ss] is a drawing whose finished strokes are [ss], in order, with
    every point clamped, consecutive duplicate points within a stroke merged,
    and empty strokes dropped. Intended for tests and for code that needs a
    drawing without simulating mouse input. *)

val strokes : t -> stroke list
(** [strokes d] is every stroke in [d], oldest first, including the stroke in
    progress (if any) as the last element. Points within each stroke are in
    drawing order. *)

val in_stroke : t -> bool
(** [in_stroke d] is [true] iff a stroke is currently in progress. *)

val is_empty : t -> bool
(** [is_empty d] is [true] if [d] contains no points at all. *)

val bounding_box : t -> (point * point) option
(** [bounding_box d] is [Some ((xmin, ymin), (xmax, ymax))], the smallest
    axis-aligned rectangle containing every point of [d] (including the stroke
    in progress), or [None] if [d] is empty. *)

val to_string : t -> string
(** [to_string d] is a representation of a drawing, one stroke per line, e.g.
    ["(10,20) (11,22) (15,30)"]. For debugging and the demo program. *)
