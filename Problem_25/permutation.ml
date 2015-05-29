let permutation (lst : 'a list) : 'a list =
  let lst' = List.map (fun x -> (Random.int (List.length lst), x)) lst in
  List.map snd (List.sort (fun (n1,_) (n2,_) -> n1 - n2) lst')
