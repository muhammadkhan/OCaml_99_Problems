let compress (lst : 'a list) : 'a list =
  let f elem acc =
    if List.length acc = 0 then [elem]
    else
      if elem = List.hd acc then acc
      else elem::acc
  in
  List.fold_right f lst []
