let length (lst : 'a list) : int =
  let rec length' lst' k =
    match lst' with
    | [] -> k
    | h::t -> length' t (k+1)
  in
  length' lst 0
