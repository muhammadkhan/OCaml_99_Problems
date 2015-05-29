let drop (lst : 'a list) : 'a list =
  let rec traverse pos = function
    | [] -> []
    | h::t ->
       if pos mod 3 = 0 then traverse (pos + 1) t
       else h::(traverse (pos + 1) t)
  in
  traverse 1 lst
