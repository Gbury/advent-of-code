
(* Sliding window *)
(* ************************************************************************* *)

module Sliding = struct

  type ('elt, 'acc) t = {
    (* Elements *)
    cur : int;
    elts : 'elt array;
    (* Accumulator *)
    acc : 'acc;
    updater : 'acc -> removed:'elt option -> added:'elt -> 'acc
  }

  let acc { acc; _ } = acc
  let full { cur; _ } = cur >= 0

  let mk ~acc ~updater ~dummy ~len () =
    let elts = Array.make len dummy in
    { elts; cur = -1; acc; updater; }

  let add { elts; cur; acc; updater; } elt =
    let pos, removed =
      if cur < 0
      then ~- (cur + 1), None
      else cur, Some elts.(cur)
    in
    let cur =
      if cur < 0
      then if cur = ~- (Array.length elts)
        then 0
        else cur -1
      else (cur + 1) mod (Array.length elts)
    in
    let acc = updater acc ~removed ~added:elt in
    let elts = Array.copy elts in
    elts.(pos) <- elt;
    { elts; cur; acc; updater; }

  let print ~print_elt ~print_acc fmt { elts; acc; _ } =
    Format.fprintf fmt "%a : %a"
      print_acc acc
      (Fmt.brackets (Fmt.array ~sep:(Fmt.any ";@ ") print_elt)) elts

end

