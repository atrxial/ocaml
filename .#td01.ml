let test x = x+1 ;;

let norme a b = sqrt(a*.a+.b*.b) ;;

let moyenne a b = (a+.b)/.2. ;;

let moyennebis a b =(float_of_int(a)+.float_of_int(b))/.2. ;;



  

let newabs a = if a < 0 then -a else a ;;



  
  


let rec suite n =
  if n = 0 then 4.
  else 3. *. suite (n-1) + 2. ;;
  
let rec ar_geo a b c n =
  if n = 0 then c
  else a *. ar_geo a b c (n-1) +. b ;;


let u n = ar_geo 3. 2. 4. n ;;



let rec somme_carre n =
  if n = 1 then 1
  else somme_carre(n-1) + n*n ;;



  
let rec puissance x n =
  if n = 0 then 1.
  else x*.puissance x (n-1) ;;

let rec puissance_bis x n =
  if n = 0 then 1.
  else if n >0 then x*.puissance_bis x (n-1) else (1./.x)*.puissance_bis x (n+1) ;;





let rec somme_liste l =
  match l with
  | [] -> 0.
  | x::r -> x +. somme_liste(r) ;;

let rec longueur l =
  match l with
  |[] -> 0
  |x::r -> 1 + longueur(r) ;;
  

let rec moyenne l = (somme_liste (l))/.float_of_int((longueur l)) ;;



let rec croissant l =
  match l with
  |[] -> true
  |[x] -> true
  | x::y::r -> x<=y && croissant (y::r) ;;

let rec  concat u v = 
  match u with 
  |[]-> v
  |x::r -> x::concat r v ;;

let rec miroir l =
  match l with
  | []-> l
  | x::r -> miroir r @[x];;




let rec unique u = 
 match u with
 |[]->[]
 |[x]->[x]
 |x::y::r-> if x=y then unique (y::r) else x::unique (y::r) ;;






let rec nb_occ x l =
  match l with
  | [] -> 0
  | a::r -> if a=x then 1 + nb_occ x r else nb_occ x r ;;



let rec procedure x l a = match l with
  |[]-> 0
  |y::r when y=x -> a - (longueur r + 1)
  |y::r -> procedure x r a ;;

let indice x l = let a = longueur l in procedure x l a ;;
  



let rec est_dedans x u = 
  match u with
  |[]->false
  |a::r -> if a=x then true else est_dedans x r ;;

let rec procedure2 u x a = match u with
|[] -> []
|y::r -> let pos=a-(longueur r +1) in if est_dedans pos x then y::procedure2 r x a else procedure2 r x a ;;

let sous_liste u x = procedure2 u x (longueur u) ;;



let rec minimum l =
  match l with
  |[] -> failwith "liste vide"
  |[x]-> x
  |x::r -> if x<= minimum r then x else minimum r ;;

let rec procedure3 l a m=
  match l with
  |[]->[]
  |y::r -> let pos=a-(longueur r+1) in if y=m then pos::procedure3 r a m else procedure3 r a m ;;

let indices_min l = procedure3 l (longueur l) (minimum l) ;;



