let rotate_once_left lst =
  match lst with
  | [] -> []
  | h::t ->  List.rev (h::(List.rev t))

let rotate (lst : 'a list) (k : int) : 'a list =
  let rec rotater lst k =
    if k = 0 then lst
    else rotater (rotate_once_left lst) (k-1)
  in
  if k < 0 then List.rev (rotater (List.rev lst) (abs k))
  else rotater lst k
