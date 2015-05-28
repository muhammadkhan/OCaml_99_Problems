let pack (lst : 'a list) : 'a list list =
  let f cur1 (acc, cur2) =
    match cur2 with
    | None -> ([[cur1]], Some(cur1))
    | Some v ->
       if v = cur1 then ((cur1::(List.hd acc))::(List.tl acc), cur2)
       else ([cur1]::acc, Some cur1)
  in
  fst (List.fold_right f lst ([], None))
