open Base
open Aoc_lib.Helpers

let day = 9
type t = IVec2.t array

let parse_input (input: string): t =
  String.split_lines input
  |> List.to_array
  |> Array.map ~f:(fun l ->
      match String.split ~on:',' l with
      | [x; y] -> (Int.of_string x, Int.of_string y)
      | _ -> failwith "Parse Error") 

let part1 (input: t): string =
  let max = ref Int.min_value in
  let n = Array.length input in

  for i = 0 to n - 2 do
    for j = i + 1 to n - 1 do
      let (x1, y1) = input.(i) and (x2, y2) = input.(j) in
      let dx = Int.abs (x2 - x1) + 1 and dy = Int.abs (y2 - y1) + 1 in
      let area = dx * dy in
      if area >= !max then max := area;
    done
  done;

  Int.to_string !max

let rotate arr =
  let arr = Array.copy arr in
  let len = Array.length arr in
  if len <= 1 then arr else
    let first = arr.(0) in
    for i = 0 to len - 2 do
      arr.(i) <- arr.(i + 1)
    done;
    arr.(len - 1) <- first; arr

let part2 (input: t): string =
  let horizontals = Array.fold2_exn input (rotate input) ~init:(Map.empty (module Int)) ~f:(fun map (sx, sy) (ex, ey) ->
    if sy = ey then Map.add_multi map ~key:sy ~data:(if sx > ex then (ex, sx) else (sx, ex)) else map)
  in

  let max = ref Int.min_value in
  let n = Array.length input in

  for i = 0 to n - 2 do
    for j = i + 1 to n - 1 do
      let (x1, y1) = input.(i) and (x2, y2) = input.(j) in
      let min_y = Int.min y1 y2 and max_y = Int.max y1 y2 in
      let min_x = Int.min x1 x2 and max_x = Int.max x1 x2 in

      if not (Map.existsi horizontals ~f:(fun ~key:y ~data:ranges ->
        y > min_y && y < max_y && List.exists ranges ~f:(fun (sx, ex) -> Int.max min_x sx < Int.min max_x ex))
      ) then (
        let dx = Int.abs (x2 - x1) + 1 and dy = Int.abs (y2 - y1) + 1 in
        let area = dx * dy in
        if area >= !max then max := area
      )
    done
  done;

  Int.to_string !max
