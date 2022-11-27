
(* Puzzle input format *)
(* ************************************************************************* *)

(* format of the line to parse *)
let format : _ format6 = ""

(* First problem solution *)
(* ************************************************************************* *)

module First = struct

  (* projection from accumulator to result/int *)
  let project n = n

  (* initial accumulator *)
  let acc = 0

  (* folding function *)
  let f _ = assert false

end

(* Second problem solution *)
(* ************************************************************************* *)

module Second = struct

  (* projection from accumulator to result/int *)
  let project n = n

  (* initial accumulator *)
  let acc = 0

  (* folding function *)
  let f _ = assert false

end

(* Main running function *)
(* ************************************************************************* *)

let () =
  Aoc.Engine.run
    ~first:(First.(Aoc.Input.fold_lines ~project ~format ~acc ~f))
    ~second:(Second.(Aoc.Input.fold_lines ~project ~format ~acc ~f))


