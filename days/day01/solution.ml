open Base
open Aoc_lib.Helpers

let day = 1

type t = int list

let parse_input (input: string): t =
  String.split_lines input
  |> List.map ~f:(fun s ->
      match String.to_list s with
      | c :: cs when Char.equal c 'L' -> Int.neg $ Int.of_string (String.of_char_list cs)
      | c :: cs when Char.equal c 'R' -> Int.of_string (String.of_char_list cs)
      | _ -> failwith "Invalid input line"
  )

let part1 (input: t): string =
  let cur = ref 50 in 
  let count = ref 0 in 

  List.iter input ~f:(fun i ->
    cur := (!cur + i) % 100;
    if !cur = 0 then count := !count + 1
  );

  !count |> Int.to_string

let part2 (input: t): string =
  List.fold_left input ~init:(50, 0) ~f:(fun (cur, count) r ->
    let rem = Int.rem r 100 in

    let zeros =
      if ((rem > 0 && cur + rem >= 100) || (rem < 0 && cur + rem <= 0 && cur > 0))
      then Int.abs (r / 100) + 1
      else Int.abs (r / 100)
    in

    ((cur + r) % 100, count + zeros)
  ) |> snd |> Int.to_string
