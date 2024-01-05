let nb_diviseurs n = ;;

let rec commun u v =
  let rec bein x a=
    match a with
    |[]-> false
    |y::r -> y=x || bein x r
  in
  match u with
  |[]->[]
  |x::r -> if bein x v then x::( commun r v) else commun r v ;;


let rec miroir u=
  match u with
  |[]->[]
  |x::r -> (miroir r)@[x];;


let liste_des_sous_listes u = 
  let a = miroir u in
  let rec proc x =
    match x with
    |[]->[]
    |y::r->  (miroir (y::r))::(proc r) 
  in
  miroir (proc a) ;;
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
let rec a_nadmet_pas_de_diviseurs_dans_b a b =
  match b with
  |[]-> failwith "vide"
  |[x]-> a mod x <> 0
  |x::r -> a mod x <> 0 && (a_nadmet_pas_de_diviseurs_dans_b a r)
  ;;
  
let rec range a b = if a=b then [] else a::(range (a+1) b);;



let est_premier n = let a = range 2 n in a_nadmet_pas_de_diviseurs_dans_b n a ;;

let rec liste_premiers n = if n=2 then [2] else if est_premier n then (liste_premiers (n-1)) @[n] else liste_premiers (n-1);;






let rec trouve_diviseur n l =
  match l with
  |[] -> failwith "vide"
  |[x] -> if n mod x = 0 then (true,x) else (false,0)
  |x::r -> if n mod x = 0 then (true,x) else trouve_diviseur n r ;;

let rec is_haming n a = if n=1 then true else let div = trouve_diviseur n a in if fst div then is_haming (n/( snd div)) a else false ;;


let haming n deg = let a = liste_premiers deg in
  let rec proc p l = if p=2 then [2] else if is_haming p l then p::(proc (p-1) l) else proc(p-1) l
  in
  proc n a ;;


let rec add_rec_terminale x y = if x = 0 then if y=0 then 0 else 1 + add_rec_terminale x (y-1) else 1+ add_rec_terminale (x-1) y ;;