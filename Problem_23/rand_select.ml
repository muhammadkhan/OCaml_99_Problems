let rec rand_select (lst : 'a list) (k : int) : 'a list =
  let rec remove_at_pos i acc = function
    | [] -> failwith "couldn't remove"
    | h::t -> if i = 0 then (h, acc @ t) else remove_at_pos (i-1) (acc @ [h]) t
  in
  if k = 0 then []
  else
    let x,xs = remove_at_pos (Random.int (List.length lst)) [] lst in
    x::(rand_select xs (k-1))
