let rec last_two: 'a list -> ('a * 'a) option = function
  | []
  | [_] -> None
  | [x1; x2] -> Some (x1, x2)
  | _::t -> last_two t
