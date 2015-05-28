let pack (lst : 'a list) : 'a list list =
  let lst' = List.map (fun x -> [x]) lst in
  let rec pack' lst =
    match lst with
    | [] 
    | [_] -> lst 
    | h1::h2::t ->
       if List.hd h1 = List.hd h2 then (h1@h2):: (pack' t)
       else h1::h2::(pack' t)
  in
  pack' lst'
