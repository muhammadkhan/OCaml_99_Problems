let range (a : int) (b : int) : int list =
  let rec generator  l u =
    if l = u then []
    else l::(generator (l+1) u)
  in
  if a < b then generator a (b+1)
  else List.rev (generator b (a+1))
