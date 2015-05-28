let encode (lst : 'a list) : (int * 'a) list =
  let rec encode' = function
    | [] -> []
    | [x] -> [x]
    | (i1,c1)::(i2,c2)::t ->
       if c1 = c2 then encode' ((i1+i2,c1)::t)
       else (i1,c1)::(encode' ((i2,c2)::t))
  in
  encode' (List.map (fun x -> (1,x)) lst)
