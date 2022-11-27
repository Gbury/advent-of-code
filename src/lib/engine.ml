
(* Input source converter *)
(* ************************************************************************* *)

type input =
  | Stdin
  | Raw of string * string
  | File of string

(* Converter for input file/stdin *)
let input_to_string input =
  match (input : input) with
  | Stdin -> "<stdin>"
  | Raw _ -> "<raw>"
  | File f -> f

let input_to_channel input =
  match (input : input) with
  | Stdin -> Scanf.Scanning.stdin
  | Raw (_, s) -> Scanf.Scanning.from_string s
  | File f -> Scanf.Scanning.from_file f

let input_source_conv =
  let parse x = Ok (File x) in
  let print fmt i = Format.fprintf fmt "%s" (input_to_string i) in
  Cmdliner.Arg.conv (parse, print)

(* Problem part selector *)
(* ************************************************************************* *)

type problem =
  | First
  | Second

let problem_to_string = function
  | First -> "first"
  | Second -> "second"


(* Main Option term *)
(* ************************************************************************* *)

type config = {
  problem : problem;
  in_channel : Scanf.Scanning.in_channel;
}

let term : config Cmdliner.Term.t =
  let open Cmdliner in
  let aux input problem =
    let in_channel = input_to_channel input in
    { in_channel; problem; }
  in
  let input =
    let doc = "Input problem file. If no file is specified, stdin will be read." in
    Arg.(value & pos 0 input_source_conv Stdin & info [] ~docv:"FILE" ~doc)
  in
  let problem =
    let first_flag =
      let doc = "Run the first part of the problem" in
      Arg.info ["first"] ~doc
    in
    let second_flag =
      let doc = "Run the second part of the problem" in
      Arg.info ["second"] ~doc
    in
    Arg.(required & vflag None [Some First, first_flag; Some Second, second_flag])
  in
  Term.(const aux $ input $ problem)


(* Run function *)
(* ************************************************************************* *)

let config () =
  let name = Format.asprintf "aoc" in
  let info = Cmdliner.Cmd.info name in
  let cli_term = Cmdliner.Cmd.v info term in
  match Cmdliner.Cmd.eval_value cli_term with
  | Ok `Ok config -> config
  | Ok ( `Version | `Help) -> exit 0
  | Error _ -> exit Cmdliner.Cmd.Exit.cli_error

let run ~first ~second =
  let { problem; in_channel; } = config () in
  let result =
    match problem with
    | First -> first in_channel
    | Second -> second in_channel
  in
  Format.printf "%d@." result

