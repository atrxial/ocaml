(* POUR LES TEST *)

let rec random_liste_int n amp = if n=0 then [] else ( Random.int amp)::(random_liste_int (n-1) amp) ;;

  
(* POUR LES TEST *)
  
  

  
  
  (*1 *)

  let rec insertion u x =
    match u with
    |[]->[x]
    |y::r -> if x<=y then x::y::r else y::(insertion r x) ;;
  
let rec insertion_sort u =
  match u with 
  |[]->[]
  |x::r -> insertion (insertion_sort r) x ;;
  
  (*2*)

let rec split u = (* faire sans longueur en ajoutant 2 trucs dans chaque composante à chaque fois*)
  let rec longueur u = 
    match u with
    |[]->0
    |x::r -> 1 + longueur r 
  in
  match u with
  |[]->([],[])
  |x::r -> match split r with
    |([],[])->([x],[])
    |(a,b) -> if longueur a <longueur b then (x::a,b) else (a,x::b) ;;
  
let rec merge u v =
  match u,v with
  |[],[]->[]
  |a,[]-> a
  |[],b -> b
  |(x::xs),(y::ys) -> if x<y then x::(merge xs (y::ys)) else y::(merge (x::xs) ys) ;;

let rec mergesort u =
  match u with
  |[]->[]
  |[x]-> [x]
  |[x;y]-> if x<=y then [x;y] else [y;x]
  |v-> let a = split v in merge (mergesort (fst a)) (mergesort (snd a)) ;;

  (*3*)

let rec partition l x =
  match l with
  |[]->([],[])
  |y::r ->  match partition r x with
    |([],[]) -> if y<=x then ([y],[]) else ([],[y])
    |(a,b) -> if y<=x then (y::a,b) else (a,y::b) ;;

let rec quicksort u =
  match u with
  |[]->[]
  |[x]-> [x]
  |[x;y]-> if x<=y then [x;y] else [y;x]
  |x::r -> let a = partition r x in (quicksort (fst a) )@ [x] @ (quicksort (snd a)) ;;



let rec bubble_sort u = 
  let rec proc u = 
    match u with
    |[]->[]
    |[x]->[x]
    |x::y::r -> if x<=y then x::(proc (y::r)) else y::(proc (x::r))
  in
  let a = proc u in if u = a then u else bubble_sort a ;;
