
(* Puzzle input format *)
(* ************************************************************************* *)

(* format of the line to parse *)
let format : _ format6 = "%s\n"

let digits s =
  Array.init (String.length s) (fun i ->
      match s.[i] with
      | '0' -> 0
      | '1' -> 1
      | c -> failwith (Format.asprintf "not a binary digit: %c" c)
    )

(* First problem solution *)
(* ************************************************************************* *)

module First = struct

  (* projection from accumulator to result/int *)
  let project (nums, bits) =
    let gamma, epsilon =
      Array.fold_left (fun (gamma, epsilon) num_one ->
          let gamma_bit, epsilon_bit =
            if num_one >= nums / 2 + 1 then 1, 0 else 0, 1
          in
          (2 * gamma + gamma_bit, 2 * epsilon + epsilon_bit)
        ) (0, 0) bits
    in
    gamma * epsilon

  (* initial accumulator *)
  let acc = (0, [||])

  (* folding function *)
  let f (nums, bits) d =
    let digits = digits d in
    if nums = 0 then (1, digits)
    else begin
      let bits =
        Array.map2 (fun num_ones digit ->
          num_ones + digit
          ) bits digits
      in
      (nums + 1, bits)
    end

end

(* Second problem solution *)
(* ************************************************************************* *)

module Second = struct

  (* projection from accumulator to result/int *)
  let project numbers =
    let rec filter index criterion = function
      | [res] ->
        Array.fold_left (fun acc bit ->
            acc * 2 + bit
          ) 0 res
      | l ->
        let n = List.length l in
        let num_ones =
          List.fold_left (fun acc digits ->
              acc + digits.(index)
            ) 0 l
        in
        let num_zeros = n - num_ones in
        let digit_to_keep =
          if num_ones >= num_zeros
          then (if criterion then 1 else 0)
          else (if criterion then 0 else 1)
        in
        let l =
          List.filter (fun digits ->
              digits.(index) = digit_to_keep
            ) l
        in
        filter (index + 1) criterion l
    in
    let oxygen = filter 0 true numbers in
    let co_two = filter 0 false numbers in
    oxygen * co_two




  (* initial accumulator *)
  let acc = []

  (* folding function *)
  let f acc s = (digits s :: acc)

end

(* Main running function *)
(* ************************************************************************* *)

let () =
  Aoc.Engine.run
    ~first:(First.(Aoc.Input.fold_lines ~project ~format ~acc ~f))
    ~second:(Second.(Aoc.Input.fold_lines ~project ~format ~acc ~f))


