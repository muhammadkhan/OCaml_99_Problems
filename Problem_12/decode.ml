type 'a rle = One of 'a | Many of int*'a

let decode (lst : 'a rle list) : 'a list =
  let rec generate k x =
    match k with
    | 0 -> []
    | _ -> x::(generate (k - 1) x)
  in
  let unzip = function
    | One(a) -> [a]
    | Many(n,a) -> generate n a
  in
  List.flatten (List.map unzip lst)
