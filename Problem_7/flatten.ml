type 'a node = One of 'a | Many of 'a node list

let rec flatten (lst : 'a node list) : 'a list =
  match lst with
  | [] -> []
  | One(x)::t -> x::(flatten t)
  | Many(nlst)::t -> (flatten nlst) @ (flatten t)
