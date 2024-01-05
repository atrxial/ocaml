(* Exercice 1*)

let rec mem u  x = match u with
  |[]-> false
  |y::ys -> y=x || mem ys x ;;

let rec nth u n = match u with
  |[]-> failwith "error"
  |x::r -> if n=(0)  then x else nth r (n-1);;

let rec nthh u n =
  match n,u with
  |0, x::r -> x
  |_,x::r -> nthh r (n-1)
  |_-> failwith "error pas assez d'éléments" ;;

let rec take u n = match u with
  |[]->[]
  |x::r -> if n>0 then x::take r (n-1) else [x] ;;


let rec range a b = if a<b then a::range (a+1) (b) else [] ;;


























(* Exercice 2*)
let rec concat u v =
  match u with
  |[]->v
  |x::r -> x::concat r v ;;


let rec miroir_naif u=
  match u  with
  |[]->[]
  |x::r-> miroir_naif r @ [x] ;;

let rev_append u v =
  match u with
  |[]->v
  |x::r -> rev_append r (x::v);;

let miroir u = rev_append u []
















(*Exercice 3*)
let rec applique u f =
  match u with
  |[]->[]
  |x::r->(f x)::applique r f ;;

let liste_carres u  = applique u (fun x->x*x);;
































(*Exercice 4*)
let rec somme u=
  match u with
  |[]-> 0
  |x::r->x+somme r ;;

let rec produit u =
  match u with
  |[]->1
  |x::r->x*produit r ;;

let rec applatit u =
  match u with
  |[]->[]
  |x::r-> x@applatit r ;;


let rec max_liste u =
  match u with
  |[]-> failwith "vide"
  |[x]->x
  |x::r -> if x>= max_liste r then x else max_liste r;;




let rec replie f u b=
  match u with
  |[] -> b
  |x::r-> f x (replie f r b) ;;



let somme2 u = replie (fun x y -> x+y) u 0 ;;
 
let produit2 u = replie (fun x y -> x*y) u 1 ;;

let applatit2 u = replie (fun x y -> x@y) u [] ;;

let max_liste u = replie max u  0 ;;










 













(*Exercice 5*)
let longueur u = replie (fun a b -> 1+b)  u 0 ;;


let variance u = let moyenne = replie (fun x y -> x+.y) u 0./.replie (fun a b->1.+.b) u 0. in
  let b = applique u (fun x->(x-.moyenne)**2.) in
    replie (fun x y->x+.y) b 0. /. replie (fun a b->1.+.b) u 0. ;;

let nb_occ x u = replie (fun a b-> if a=x then 1+b else b) u 0 ;;


(*Exercice 6*)
let sum (a,b) (x,y) = (a+x,b+y) ;;

let rec compresse u =
  match u with
  |[]->[]
  |x::r ->
      match compresse r with
        |[]->[(x,1)]
        |(a,b)::v when a=x -> (a,b+1)::v
        |v-> (x,1)::v  ;;

let rec miseliste p = match p with | (a,b) -> if b=0 then [] else a::(miseliste (a,b-1)) ;;

let rec decompresse u =
  match u with
  |[]->[]
  |(a,b)::v-> (miseliste (a,b))@decompresse v ;;

(*Exercice 7*) (* peu générale pour le moment*)

let rec enleve_multiples n u = 
  match u with
  |[]->[]
  |x::r-> if x mod n = 0 then enleve_multiples n r else x::enleve_multiples n r ;;