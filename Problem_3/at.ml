let rec at (k : int) (lst : 'a list) : 'a option =
  match k,lst with
  | _, [] -> None
  | 0, h::_ -> Some h
  | k, h::t -> at (k-1) t
