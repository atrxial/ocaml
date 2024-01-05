type arbre =
  |Nil
  |Noeud of bool * arbre * arbre ;;



let rec cherche n a = match a with
  |Nil -> false
  |Noeud (b,x,y) when n = 0 -> b 
  |Noeud (b,x,y) when n mod 2 = 0 -> cherche (n/2) x 
  |Noeud (b,x,y) -> cherche (n/2) y ;;




let rec ajoute n a = match a with
  |Nil when n = 0 -> Noeud (true,Nil,Nil) 
  |Nil when n mod 2 = 0 -> Noeud (false,ajoute (n/2) Nil, Nil) 
  |Nil -> Noeud (false,Nil,ajoute (n/2) Nil)
  |Noeud (b,x,y) when n = 0 -> Noeud (true,x,y)
  |Noeud (b,x,y) when n mod 2 = 0 -> Noeud (b,ajoute (n/2) x,y)
  |Noeud (b,x,y) -> Noeud (b,x, ajoute (n/2) y) ;;


let rec construit l = match l with
  |[]-> Nil
  |x::r -> ajoute x (construit r ) ;;

let construit_term l = 
  let rec aux u a = match u with
    |[]->a
    |x::r -> aux r ( ajoute x a )
  in aux l Nil ;;



let rec supprime n a = match a with
  |Nil -> a 
  |Noeud (b,x,y) when n = 0 -> Noeud (false,x,y)
  |Noeud (b,x,y) when n mod 2 = 0 -> Noeud (b,supprime (n/2) x ,y)
  |Noeud (b,x,y) -> Noeud (b,x, supprime (n/2) y) ;;


let rec union a b = match a,b with
  |Nil,ens | ens, Nil -> ens
  |Noeud (b,x,y), Noeud (bb,xx,yy) -> Noeud (b || bb , union x xx , union y yy);;




let rec intersection a b = match a,b with
  |Nil,ens | ens, Nil -> Nil
  |Noeud (b,x,y), Noeud (bb,xx,yy) -> Noeud (b && bb , intersection x xx , intersection y yy);;


let rec ajouter_1 u = match u with
  |[] -> []
  |x::r -> (1::x)::( ajouter_1 r) ;;


let rec ajouter_0 u = match u with
  |[] -> []
  |x::r -> (0::x)::( ajouter_0 r) ;;

let recup u = match u with
  |[]->[]
  |x::r -> x ;;

let rec binar a = match a with 
  |Nil -> []
  |Noeud (b,x,y) when b -> let p,q = binar x, binar y in ajouter_0 p @ [(recup p)] @ ajouter_1 q
  |Noeud (b,x,y)-> let p,q = binar x, binar y in ajouter_0 p @ ajouter_1 q ;;

let to_int u = 
  let rec aux u q s = match u with
  |[]-> s
  |x::r -> aux r (2*q) (s+x*q)
  in aux u 1 0 ;;

let elements a = List.map to_int (binar a) ;;









let rec elague a = 
  let aux a = match a with
  |Nil -> Nil
  |Noeud (false,Nil,Nil) -> Nil
  |Noeud (b,x,y) -> Noeud (b,elague x ,elague y)
  in if a<>aux a then elague (aux a) else a ;;