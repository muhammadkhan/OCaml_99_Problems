let split (lst : 'a list) (k : int) : 'a list * 'a list =
  let splitter elem (l1, l2, pos) =
    if pos > k then (l1, elem::l2, pos-1)
    else (elem::l1, l2, pos-1)
  in
  let (a,b,_) = List.fold_right splitter lst ([], [], List.length lst) in
  (a,b)
