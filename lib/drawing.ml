let width = 280
let height = 280

type point = int * int
type stroke = point list

(* Representation: [finished] holds the completed strokes, newest first, and
   [current] holds the stroke in progress, if any. To make appending O(1), the
   points of every stored stroke are kept in REVERSE drawing order (most recent
   point first).

   Abstraction function: the drawing whose strokes, oldest first, are
   [List.rev_map List.rev (finished)] followed by [List.rev c] when [current =
   Some c].

   Representation invariant: every stored stroke is non-empty; every point
   satisfies 0 <= x < width and 0 <= y < height; no two consecutive points in a
   stroke are equal. *)
type t = {
  finished : point list list;
  current : point list option;
}

let empty = { finished = []; current = None }

let clamp (x, y) =
  let clip lo hi v = max lo (min hi v) in
  (clip 0 (width - 1) x, clip 0 (height - 1) y)

let end_stroke d =
  match d.current with
  | None -> d
  | Some c -> { finished = c :: d.finished; current = None }

let start_stroke p d =
  let d = end_stroke d in
  { d with current = Some [ clamp p ] }

let add_point p d =
  match d.current with
  | None -> d
  | Some c -> (
      let p = clamp p in
      match c with
      | last :: _ when last = p -> d
      | _ -> { d with current = Some (p :: c) })

let clear _ = empty

let of_strokes ss =
  List.fold_left
    (fun d s ->
      match s with
      | [] -> d
      | p :: rest ->
          start_stroke p d |> fun d ->
          List.fold_left (fun d q -> add_point q d) d rest |> end_stroke)
    empty ss

let strokes d =
  let newest_first =
    match d.current with
    | None -> d.finished
    | Some c -> c :: d.finished
  in
  List.rev_map List.rev newest_first

let in_stroke d = d.current <> None
let is_empty d = d.finished = [] && d.current = None

let bounding_box d =
  match List.concat (strokes d) with
  | [] -> None
  | (x, y) :: rest ->
      let x0, y0, x1, y1 =
        List.fold_left
          (fun (x0, y0, x1, y1) (x, y) ->
            (min x0 x, min y0 y, max x1 x, max y1 y))
          (x, y, x, y) rest
      in
      Some ((x0, y0), (x1, y1))

let to_string d =
  let point_str (x, y) = Printf.sprintf "(%d,%d)" x y in
  strokes d
  |> List.map (fun s -> String.concat " " (List.map point_str s))
  |> String.concat "\n"
