let rev (lst: 'a list) : 'a list =
  let rec rev' l1 l2 =
    match l1 with
    | [] -> l2
    | h::t -> rev' t (h::l2)
  in
  rev lst []
