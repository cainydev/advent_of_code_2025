open Base
open Aoc_lib.Helpers

let day = 6
type t = string list

let parse_input (input: string): t = String.split_lines input

let part1 (lines: t): string =
  let ta s = String.split ~on:' ' s |> List.filter ~f:(Fn.non String.is_empty) in

  let numbers =
    List.take lines (List.length lines - 1)
    |> List.map ~f:(fun l -> ta l |> List.map ~f:Int.of_string)
    |> List.transpose_exn
  in
  let ops = List.last_exn lines |> ta |> List.map ~f:Char.of_string in

  List.sum (module Int) (List.zip_exn numbers ops) ~f:(fun (nums, op) ->
    match op with
    | '+' -> List.sum (module Int) nums ~f:Fn.id
    | '*' -> List.fold nums ~init:1 ~f:( * )
    | _ -> failwith "Unknown operation"
  ) |> Int.to_string

let part2 (input: t): string =
  let grid = List.map input ~f:(String.to_array) |> List.to_array in
  let number_rows = Array.length grid - 1 in
  let task_l =
    String.of_array grid.(number_rows)
    |> String.split_on_chars ~on:['+'; '*']
    |> List.tl_exn
    |> List.map ~f:(fun s -> String.length s)
    |> List.to_array
  in
  let num_tasks = Array.length task_l in
  task_l.(num_tasks - 1) <- task_l.(num_tasks - 1) + 1;

  let sum = ref 0 in
  let x = ref 0 in

  for t = 0 to num_tasks - 1 do
    let l = task_l.(t) in
    let op =
      match grid.(number_rows).(!x) with
      | '+' -> ( + )
      | '*' -> ( * )
      | c -> failwith (String.of_list [c])
    in

    let res = ref 0 in
    if op !res 2 = !res then res := 1;

    for xs = !x to !x + l - 1 do
      let num = ref 0 in
      let pos = ref 0 in

      for y = number_rows - 1 downto 0 do 
        if not (Char.is_whitespace grid.(y).(xs))
        then (
          num := !num + Char.get_digit_exn grid.(y).(xs) * (Int.pow 10 !pos);
          pos := !pos + 1
        )
      done;
      
      res := op !res !num
    done;

    sum := !sum + !res;
    x := !x + l + 1;
  done;

  !sum |> Int.to_string
