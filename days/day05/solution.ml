open Base
open Aoc_lib.Helpers

let day = 5
type t = (int * int) list * (int list)

let parse_input (input: string): t =
  String.split_lines input
  |> List.fold ~init:([], []) ~f:(fun (ranges, ids) str ->
      if String.is_empty str then
        (ranges, ids)
      else
        if String.contains str '-' then
          let parts = String.split str ~on:'-' in
          match parts with
          | [start_str; end_str] ->
            let start_int = Int.of_string start_str in
            let end_int = Int.of_string end_str in
            ((start_int, end_int) :: ranges, ids)
          | _ -> (ranges, ids)
        else
          let id = Int.of_string str in
          (ranges, id :: ids)
  )

let merge_ranges ranges =
  let rec aux = function
    | [] | [_] as l -> l
    | (s1, e1) :: (s2, e2) :: rest ->
        if e1 >= s2 
        then aux ((s1, Int.max e1 e2) :: rest)
        else (s1, e1) :: aux ((s2, e2) :: rest)
  in aux (List.sort ranges ~compare:(fun (s1, _) (s2, _) -> Int.compare s1 s2))

let part1 ((ranges, ids): t): string =
  let merged_ranges = merge_ranges ranges in
  List.count ids ~f:(fun id ->
    List.exists merged_ranges ~f:(fun (start_int, end_int) ->
      id >= start_int && id <= end_int
    )
  )
  |> Int.to_string

let part2 ((ranges, _): t): string =
  merge_ranges ranges 
  |> List.sum (module Int) ~f:(fun (s, e) -> e - s + 1)
  |> Int.to_string
