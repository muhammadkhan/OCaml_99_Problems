let rec extract (n : int) (lst : 'a list) : 'a list list =
  match lst with
  | [] -> []
  | h::t ->
     if n = 1 then List.map (fun x -> [x]) lst
     else 
     let lst' = List.map (fun x -> h::x) (extract (n - 1) t) in
     lst'@(extract n t)
