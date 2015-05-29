let slice (lst : 'a list) (i : int) (k : int) : 'a list =
  let rec traverse pos = function
    | [] -> []
    | h::t -> if i <= pos && pos <= k then h::(traverse (pos+1) t)
	      else traverse (pos + 1) t
  in
  traverse 0 lst
