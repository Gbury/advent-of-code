
let stdin = Scanf.Scanning.stdin

let rec fold_lines ~project ~format ~acc ~f in_channel =
  match Scanf.bscanf in_channel format (f acc) with
  | acc' -> fold_lines ~project ~format ~acc:acc' ~f in_channel
  | exception End_of_file -> project acc

