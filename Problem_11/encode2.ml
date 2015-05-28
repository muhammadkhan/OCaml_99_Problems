type 'a rle = One of 'a | Many of int*'a

let encode (lst : 'a list) : 'a rle list =
  let rec encode' = function
    | [] -> []
    | [x] -> [x]
    | (i1, c1)::(i2,c2)::t ->
       if c1 = c2 then encode' ((i1+i2,c1)::t)
       else (i1,c1)::(encode' ((i2,c2)::t))
  in
  List.map (fun (n,a) -> if n = 1 then One a else Many (n,a)) (encode' (List.map (fun x -> (1,x)) lst))
