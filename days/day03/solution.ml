open Base
open Aoc_lib.Helpers

let day = 3
type t = int list list

let parse_input (input: string): t =
  String.split_lines input
  |> List.map ~f:(fun line ->
    String.to_list line
    |> List.map ~f:(Char.get_digit_exn)
  )

let part1 (input: t): string =
  List.sum (module Int) input ~f:(fun b ->
    let last = List.length b - 1 in
    List.foldi b ~init:(0, 0) ~f:(fun i acc c ->
      match acc with
      | (l, r) when i <> last && c > l -> (c, 0)
      | (l, r) when c > r -> (l, c)
      | otherwise -> acc
    )
    |> (fun (l, r) -> l * 10 + r)
  )
  |> Int.to_string

let part2 (input: t): string =
  let rec clean stack stack_len n drop =
    match stack with
    | [] -> ([n], 1)
    | x :: xs when drop > 0 && x < n -> clean xs (stack_len - 1) n (drop - 1)
    | stack -> (n :: stack, stack_len + 1)
  in

  List.sum (module Int) input ~f:(fun b -> 
    let total = List.length b in
    List.foldi b ~init:([], 0) ~f:(fun i (stack, stack_len) c ->
      clean stack stack_len c ((total - i) - (12 - stack_len))
    )
      |> fst
      |> List.rev
      |> (fun l -> List.take l 12)
      |> List.fold ~init:0 ~f:(fun acc digit -> acc * 10 + digit)
  ) |> Int.to_string
