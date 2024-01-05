(* let rec par_division a n =
  if n=1 then a
  else let q = par_division a (n/2) in if n mod 2 = 0 then q*q else a*q*q ;;







let rec binaire_inverse n =
  if n = 0 then [] else (n mod 2) :: binaire_inverse (n/2) ;;


let par_decomposition_binaire n =
  let rec aux u =
    match u with
    |[]->([],1,0)
    |x::r-> let l,q,s= aux r in if x=0 then (l,q*q,s) else match l with |[]->([1],1,1) |y::ys-> ((q+s)::q::ys,q*q,s+s+q)
  in let a,b,c= (aux (binaire_inverse n) ) in a;;






*)












let rec par_division n = if n = 1 then [n] else
  if n mod 2 = 0 then n ::( par_division (n/2))
  else n::(par_division (n-1)) ;;



let rec binaire_inverse n = if n <= 1 then [n]
  else (n mod 2)::(binaire_inverse (n/2)) ;;





let rec par_decomposition_binaire n = 
  let rec aux a u v s =
    match u with 
    |[]-> v
    |x::r when x = 1 -> if s=0  then aux (2*a) r ((s+a)::v) (s+a) else aux (2*a) r ((s+a)::a::v) (s+a)
    |x::r -> aux (2*a) r (a::v) s
  in aux 1 (binaire_inverse n) [] 0 ;;


let chercher_indices t k = 
  let i = ref 0 in 
  let j = ref 0 in 
  let not_trouve = ref true in 
  while !i<=k && !not_trouve do
    let j = ref 0 in
    while !j<=k && !not_trouve do
      if t.(!i)+t.(!j) = t.(k) then not_trouve := false else  j:= !j+1 
    done ; i:=!i+1
done ; (!i,!j) ;;


let puissance x t =
  let rec aux x t (i,j) = match i,j with
    |0,0 -> x*.x
    |_ -> let a,b = chercher_indices t i , chercher_indices t j in  (aux x t a) *. (aux x t b) 
  in aux x t (chercher_indices t (Array.length t -1) );;






