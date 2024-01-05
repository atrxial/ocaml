let rec compte n u = match u with
  |[] -> 0
  |x::r when n=x -> 1 + compte n r 
  |x::r when n>x -> compte n r + compte (n-x) u 
  |x::r -> compte n r ;;

let ajoute a u = List.map (fun x-> a::x) u ;;

let rec compte_affiche n u = match u with
  |[] -> []
  |x::r when n=x -> [[x]] @ compte_affiche n r 
  |x::r when n>x -> compte_affiche n r @ ajoute x (compte_affiche (n-x) u)
  |_::r -> compte_affiche n r ;;

let rec test u = 
  let rec somme u = match u with
  |[]->0
  |x::r-> x + somme r in
  match u with
  |[] -> failwith "vide"
  |[x] -> print_newline (print_int (somme x))
  | x::r -> print_newline (print_int (somme x)) ; test r ;;