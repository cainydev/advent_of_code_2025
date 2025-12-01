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
  let cur = ref 50 in 
  let count = ref 0 in 

  List.iter input ~f:(fun r ->
    count := !count + Int.abs (r / 100);
    let x_rest = Int.rem r 100 in

    if (x_rest > 0 && !cur + x_rest >= 100) then count := !count + 1
    else if (x_rest < 0 && !cur + x_rest <= 0 && !cur > 0) then count := !count + 1;

    cur := (!cur + r) % 100;

    Stdio.printf "Move: %d, New Pos: %d, Count: %d\n" r !cur !count
  );

  !count |> Int.to_string
