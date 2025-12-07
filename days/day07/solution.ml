open Base
open Aoc_lib.Helpers

let day = 7
type t = bool Grid.t

let parse_input (input: string): t =
  String.split_lines input
  |> List.to_array
  |> Array.map ~f:(fun line ->
         String.to_array line
         |> Array.map ~f:(function
              | '^' -> true
              | '.' | 'S' -> false
              | _ -> failwith "Invalid character in input"))

let part1 (input: t): string =
  let start = Set.singleton (module Int) (Grid.width input / 2) in
  let height = Grid.height input in

  let rec count_splits beams y splits =
    if y >= height then splits else
      let (newBeams, newSplits) =
        Set.fold beams ~init:(Set.empty (module Int), splits) ~f:(fun (acc, split_count) x ->
          if input.(y).(x)
          then (Set.add acc (x - 1) |> (Fn.flip Set.add) (x + 1), split_count + 1)
          else (Set.add acc x, split_count)
        )
      in count_splits newBeams (y + 1) newSplits
  in
  count_splits start 0 0 |> Int.to_string

let part2 (input: t): string =
  let start = Array.mapi input.(0) ~f:(fun i _ -> if i = (Grid.width input / 2) then 1 else 0) in
  let width = Grid.width input in 
  
  Array.fold input ~init:start ~f:(fun timelines splitters ->
    let newTimelines = Array.create ~len:width 0 in
    for x = 0 to width - 1 do 
      if splitters.(x) then (
        newTimelines.(x - 1) <- newTimelines.(x - 1) + timelines.(x);
        newTimelines.(x + 1) <- newTimelines.(x + 1) + timelines.(x)
      ) else (
        newTimelines.(x) <- newTimelines.(x) + timelines.(x)
      )
    done;

    newTimelines
  )

  |> Array.sum (module Int) ~f:Fn.id |> Int.to_string
