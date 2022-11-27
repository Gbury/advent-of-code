
(* Puzzle input format *)
(* ************************************************************************* *)

(* format of the line to parse *)
let format : _ format6 = " %d\n"

(* First problem solution *)
(* ************************************************************************* *)

module First = struct

  (* projection from accumulator to result/int *)
  let project (_, n) = n

  (* initial accumulator:
     last depth seen * number of changes *)
  let acc =
    None, 0

  (* folding function *)
  let f (last_depth, n) new_depth =
    match last_depth with
    | None -> Some new_depth, n
    | Some depth ->
      let n = if new_depth > depth then n + 1 else n in
      Some new_depth, n

end

(* Second problem solution *)
(* ************************************************************************* *)

module Second = struct

  (* projection from accumulator to result/int *)
  let project (_, n) = n

  (* initial accumulator:
     last depth seen * number of changes *)
  let acc =
    let updater acc ~removed ~added =
      let acc = acc + added in
      match removed with
      | None -> acc
      | Some i -> acc - i
    in
    let w =
      Aoc.Window.Sliding.mk ()
        ~acc:0 ~updater ~dummy:0 ~len:3
    in
    w, 0

  (* folding function *)
  let f (w, n) depth =
    let full = Aoc.Window.Sliding.full w in
    let w' = Aoc.Window.Sliding.add w depth in
    let n =
      if not full then n
      else begin
        let old = Aoc.Window.Sliding.acc w in
        let cur = Aoc.Window.Sliding.acc w' in
        if cur > old then n + 1 else n
      end
    in
    (w', n)

end

(* Main running function *)
(* ************************************************************************* *)

let () =
  Aoc.Engine.run
    ~first:(First.(Aoc.Input.fold_lines ~project ~format ~acc ~f))
    ~second:(Second.(Aoc.Input.fold_lines ~project ~format ~acc ~f))


