
(* Puzzle input format *)
(* ************************************************************************* *)

(* format of the line to parse *)
let format : _ format6 = "%s@ %d\n"

type direction =
  | Forward
  | Down | Up

let direction = function
  | "forward" -> Forward
  | "down" -> Down
  | "up" -> Up
  | s -> failwith (Format.asprintf "not a direction: %s" s)

(* First problem solution *)
(* ************************************************************************* *)

module First = struct

  (* projection from accumulator to result/int *)
  let project (h, depth) = h * depth

  (* initial accumulator:
     horizontal pos * depth *)
  let acc = 0, 0

  (* folding function *)
  let f (h, depth) dir n =
    match direction dir with
    | Forward -> (h + n, depth)
    | Down -> (h, depth + n)
    | Up -> (h, depth - n)

end

(* Second problem solution *)
(* ************************************************************************* *)

module Second = struct

  (* projection from accumulator to result/int *)
  let project (h, depth, _aim) = h * depth

  (* initial accumulator:
     last depth seen * number of changes *)
  let acc = 0, 0, 0

  (* folding function *)
  let f (h, depth, aim) dir x =
    match direction dir with
    | Forward -> (h + x, depth + (aim * x) , aim)
    | Down -> (h, depth, aim + x)
    | Up -> (h, depth, aim - x)

end

(* Main running function *)
(* ************************************************************************* *)

let () =
  Aoc.Engine.run
    ~first:(First.(Aoc.Input.fold_lines ~project ~format ~acc ~f))
    ~second:(Second.(Aoc.Input.fold_lines ~project ~format ~acc ~f))


