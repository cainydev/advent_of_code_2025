open Base
open Aoc_lib.Helpers

let day = 8

module Point = struct
  module T = struct
    type t = int * int * int [@@deriving compare, sexp, hash]
  end

  include T

  include Comparator.Make(T)
  
  let dist_sq (x1, y1, z1) (x2, y2, z2) =
    let dx = x1 - x2 in
    let dy = y1 - y2 in
    let dz = z1 - z2 in
    (dx * dx) + (dy * dy) + (dz * dz)
end

type t = Point.t array

module Edge = struct
  type t = {
    p1 : Point.t;
    p2 : Point.t;
    dist : int;
  }

  let compare a b = Int.compare a.dist b.dist
  
  let create ?dist p1 p2  =
    let dist = match dist with
    | Some d -> d
    | None -> Point.dist_sq p1 p2
  in { p1; p2; dist }
end

let sort ((x1, y1, z1) as p1) ((x2, y2, z2) as p2) =
  if Float.(x1 > x2 || y1 > y2 || z1 > z2) then (p2, p1) else (p1, p2)

let parse_input (input: string): t =
  String.split_lines input
  |> List.map ~f:(String.split ~on:',' >> List.map ~f:Int.of_string >> function
       | [x; y; z] -> (x, y, z)
       | _ -> failwith "Invalid input format")
  |> List.to_array

let part1 (input: t): string =
  let heap = Pairing_heap.create ~min_size:1000 ~cmp:(fun a b -> Edge.compare b a) () in
  let count = ref 0 in

  for i = 0 to Array.length input - 1 do
    for j = i + 1 to Array.length input - 1 do
      let dist = Point.dist_sq input.(i) input.(j) in

      if !count < 1000 then (
        Pairing_heap.add heap (Edge.create ~dist input.(i) input.(j));
        Int.incr count
      )
      else
        if dist < (Pairing_heap.top_exn heap).dist
        then (
          Pairing_heap.remove_top heap;
          Pairing_heap.add heap (Edge.create ~dist input.(i) input.(j))
        );
    done
  done;

  let adj = Pairing_heap.fold heap ~init:(Map.empty (module Point)) ~f:(fun adj edge ->
    let adj = Map.change adj edge.p1 ~f:(
      function
      | None -> Some (Set.singleton (module Point) edge.p2)
      | Some ns -> Some (Set.add ns edge.p2)
    ) in
    
    Map.change adj edge.p2 ~f:(
      function
      | None -> Some (Set.singleton (module Point) edge.p1)
      | Some ns -> Some (Set.add ns edge.p1)
    )
  )
  in

  Map.fold adj ~init:([], Set.empty (module Point)) ~f:(fun ~key ~data (components, visited_total) ->
    if Set.mem visited_total key then (components, visited_total) else

    let q = Queue.singleton key in
    let visited = Hash_set.create (module Point) in

    Hash_set.add visited key;

    while not (Queue.is_empty q) do
      let p = Queue.dequeue_exn q in
      
      if Map.mem adj p then (
        Set.iter (Map.find_exn adj p) ~f:(fun other ->
          if not (Hash_set.mem visited other) then (
            Queue.enqueue q other;
            Hash_set.add visited other
          )
        )
      )
    done;

    let newVisited = (Set.of_array (module Point) (Hash_set.to_array visited)) in

    ((Hash_set.length visited) :: components, Set.union visited_total newVisited)
  )
  |> fst
  |> List.sort ~compare:Int.descending 
  |> (Fn.flip List.take) 3
  |> List.fold ~init:1 ~f:(fun acc group -> acc * group)
  |> Int.to_string

let part2 (input: t): string =
  let n = Array.length input in
  let edges = Array.create ~len:(n * (n-1) / 2) (Edge.create (0, 0, 0) (0, 0, 0)) in
  
  let index = ref 0 in
  for i = 0 to n - 1 do
    for j = i + 1 to n - 1 do
      edges.(!index) <- Edge.create input.(i) input.(j);
      Int.incr index
    done
  done;

  let heap = Pairing_heap.of_array edges ~cmp:(fun a b -> Edge.compare a b) in

  let groups = Hashtbl.create (module Point) in
  let index = ref 0 in
  let sizes = Array.create ~len:n 0 in
  let solution = ref 0 in
  let largest = ref 0 in

  while !largest < n do
    let edge = Pairing_heap.pop_exn heap in

    match Hashtbl.find groups edge.p1 with
    | None -> (
      match Hashtbl.find groups edge.p2 with
      | None -> (
        Hashtbl.add_exn groups ~key:edge.p1 ~data:!index;
        Hashtbl.add_exn groups ~key:edge.p2 ~data:!index;
        sizes.(!index) <- 2;
        if !largest < 2 then largest := 2;
        Int.incr index 
      )
      | Some group -> (
        Hashtbl.add_exn groups ~key:edge.p1 ~data:group;
        sizes.(group) <- sizes.(group) + 1;
        if !largest < sizes.(group) then largest := sizes.(group)
      )
    )
    | Some group -> (
      match Hashtbl.find groups edge.p2 with
      | None -> (
        Hashtbl.add_exn groups ~key:edge.p2 ~data:group;
        sizes.(group) <- sizes.(group) + 1;
        if !largest < sizes.(group) then largest := sizes.(group)
      )
      | Some other_group -> (
        if group <> other_group then (
          let (smaller, bigger) =
            if sizes.(group) < sizes.(other_group)
            then (group, other_group)
            else (other_group, group)
          in
          Hashtbl.mapi_inplace groups ~f:(fun ~key ~data -> if data = smaller then bigger else data);
          sizes.(bigger) <- sizes.(bigger) + sizes.(smaller);
          if sizes.(bigger) > !largest then largest := sizes.(bigger)
        )
      )
    );
    
    solution := (fst3 edge.p1) * (fst3 edge.p2)
  done;

  Int.to_string !solution
