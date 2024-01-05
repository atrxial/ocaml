let rec double u =
  match u with
  |[]->[]
  |x::r -> x::x::double r ;;

let rec repete u n =
  match u,n with
  |[],_ -> failwith "vide"
  |v,x when x=1 -> v
  |v,x -> v@repete u (n-1) ;;

let monotone u = (* faire avec une méthode qui calcule de base le premier écart et qui regarde la suite*)
  let rec croissante u =
    match u with
    |[]-> true
    |[x]-> true
    |x::y::r -> x<=y && croissante (y::r)
  in 
  let rec decroissante u =
    match u with
    |[]-> true
    |[x]-> true
    |x::y::r -> x<=y && decroissante (y::r)
  in
  croissante u || decroissante u ;;

let rec partition f u =
  match u with
  |[]->([],[])
  |x::r -> match partition f r with
    |(a,b) when f x -> (x::a,b)
    |(a,b)-> (a,x::b) ;;

let rec zip a b =
  match a,b with
  |[],[]-> []
  |x::r,y::rr -> (x,y):: zip r rr
  | _ -> failwith " longueurs différentes" ;;

let rec miroir u=
  match u with
  |[]->[]
  |x::r -> miroir (r)@[x];;

let sommes_cumulees u = let a = miroir u in
  let rec proc l =
    match l with
    |[]->[]
    |x::r-> match proc r with
      |[]->[x]
      |y::ys-> (x+y)::y::ys
  in
  miroir (proc a) ;;

let sommes_cumulees_fold u = List.fold_left (fun x y -> match miroir x with 
  |a::r -> miroir((y+a)::a::r)
  |[]->[y]) [] u;;

let rec enleve_doublons u =
  match u with
  |[]->[]
  |[x]->[x]
  |x::y::r when x=y -> enleve_doublons (y::r)
  |x::y::r -> x::enleve_doublons (y::r) ;;







let rec chiffres n b = if n=0 then [] else (n mod b)::( chiffres (n/b) b) ;;

let rec nombres l b =
  match l with
  |[]-> 0
  |x::r -> x+b*(nombres r b) ;;

let convertit l b1 b2 = chiffres (nombres l b1) b2 ;;





(*izi*)


let rec ajout page x l =
  match l with
  |[]->[(x,[page])]
  |(a,b)::r -> if a=x then (a,page::b)::r else (a,b)::(ajout page x r) ;;

let rec make_index u =
  match u with
  |[]->[]
  |(_,[])::r -> make_index r
  |(pag,x::xr)::r1 -> ajout pag x (make_index ((pag,xr)::r1)) ;;                                              


  
let rec produit_cartesien u v =
  let rec proc a l=
    match l with 
    |[]->[]
    |y::r-> (a,y)::(proc a r)
  in
  match u with
  |[]-> []
  |x::r -> (proc x v )@(produit r v) ;;






let rec somme l = match l with |[]-> 0 |x::r -> x+ somme r ;;

let rec normalize u b = if List.length u >=b then u else (normalize u (b-1))@[0];;

let combi_repet n m = let rec proc lim n m nb = if n = lim then [] else let a = normalize (chiffres n m) m in let s = somme a in if s = nb then a::(proc lim (n+1) m nb) else proc lim (n+1) m nb
  in proc (int_of_float(float_of_int(m)**float_of_int(m))-1) 1 m n;;






let rec ajout_x_a_pos x pos l = if pos = 0 then x::l else match l with |[]->[] |a::b-> a::(ajout_x_a_pos x (pos-1) b) ;;

let ajout_x_partout x p = let rec proc x n u = if n = 0 then [ajout_x_a_pos x n u] else (ajout_x_a_pos x n u) :: (proc x (n-1) u ) in
  proc x (List.length p) p;;

let rec ajout_x_liste x l = match l with |[]->[] |a::r -> (ajout_x_partout x a )@( ajout_x_liste x r);;


let rec permutation u = match u with
    |[]-> [[]]
    |x::xs -> ajout_x_liste x (permutation xs) ;;


let rec elimination u = 
  let rec proc v =
    match v with 
      |[]->[]
      |[w]->[w]
      |x::y::r -> if x=y then proc r else x::(proc (y::r))
  in
  let a = proc u in if a = u then a else elimination a ;;