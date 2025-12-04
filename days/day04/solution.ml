open Base
open Aoc_lib.Helpers

let day = 4
type t = bool Grid.t

let parse_input (input: string): t =
  String.split_lines input
  |> List.map ~f:(fun line ->
         String.to_array line
         |> Array.map ~f:(function
              | '@' -> true
              | '.' -> false
              | _ -> failwith "Invalid character in input"
          )
      )
  |> List.to_array

let step map = 
  Grid.map (fun pos state ->
    state && (Grid.fold8 (fun _ n acc -> if n then acc + 1 else acc) map pos 0) >= 4
  ) map

let count map =
  Grid.fold (fun _ state acc -> if state then acc + 1 else acc) map 0

let part1 (input: t): string =
  (count input - count (step input)) |> Int.to_string

let step_further map =
  let rec aux map map_count =
    let res = step map in
    let res_count = count res in
    if res_count = map_count
    then res else aux res res_count
  in
  aux map (count map)

let part2 (input: t): string =
  (count input - count (step_further input)) |> Int.to_string
