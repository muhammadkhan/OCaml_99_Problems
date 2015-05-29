let replicate (lst : 'a list) (k : int) : 'a list =
  let rec generate x = function
    | 0 -> []
    | k -> x::(generate x (k-1))
  in
  List.flatten (List.map (fun x -> generate x k) lst)
