let remove_at (k : int) (lst : 'a list) : 'a list =
  let rec traverse pos = function
    | [] -> []
    | h::t -> if pos = k then traverse (pos+1) t else h::(traverse (pos+1) t)
  in
  traverse 0 lst
