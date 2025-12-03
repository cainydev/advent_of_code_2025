open Base
open Aoc_lib.Helpers

let day = 2
type t = (int * int) list

let parse_input (input: string): t =
  String.split ~on:',' input
  |> List.map ~f:(fun l ->
    match String.split ~on:'-' l with
    | [s; e] -> (Int.of_string s, Int.of_string e)
    | _ -> failwith "Parse error"
  )

let repeats i =
  let s = Int.to_string i |> String.to_list in
  let l = List.length s in
  l % 2 = 0 && List.equal (Char.equal) (List.take s (l / 2)) (List.drop s (l / 2))

let part1 (input: t): string =
  List.sum (module Int) ~f:(fun (s, e) ->
    let invalids = ref 0 in
    for n = s to e do
      if repeats n then invalids := !invalids + n
    done;
    !invalids
  ) input
  |> Int.to_string

let repeats_many i =
  let s = Int.to_string i |> String.to_list in
  let l = List.length s in

  let repeats = ref false in

  for i = 1 to l / 2 do
    if
      List.all_equal (List.chunks_of s ~length:i) ~equal:(List.equal Char.equal)
      |> Option.is_some
    then
      repeats := true
  done;

  !repeats

let part2 (input: t): string =
    List.sum (module Int) ~f:(fun (s, e) ->
    let invalids = ref 0 in
    for n = s to e do
      if repeats_many n then invalids := !invalids + n
    done;
    !invalids
  ) input
  |> Int.to_string
