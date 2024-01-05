(* -*- coding: utf-8 -*- *)

(***************************
******** Partie 1 **********
***************************)

type couleur = Pique | Coeur | Carreau | Trefle 
type valeur = As | Roi | Dame | Valet | Mineure of int
type carte = {co : couleur; va: valeur}

let valide carte = match carte.va with
  |Mineure n -> 2<=n && n<=10
  | _-> true ;;


let valeur atout carte =
     match carte with
     |{va = As} -> 11
     |{va = Roi} -> 4
     |{va = Dame} -> 3
     |{va = Mineure 10} -> 10
     |{co = x ; va = Mineure 9 } -> if x = atout then  14 else 0
     |{co = x ; va = Valet } -> if x = atout then 20 else 2
     |_-> 0 ;;



type contrat = {atout : couleur; score : int}

let resultat contrat plis =
  let rec somme atout l =
    match l with
    |[]->0
    |x::r -> valeur atout x + somme atout r in
  let a,b=contrat.atout,contrat.score in
  let delta = somme a plis - b in if delta >=0  then (true,delta) else (false,delta) ;;



(* Quand vous appelez "verif_partie_1 ()", vous devez obtenir
   - : unit = ()
   Si vous obtenez une exception, c'est qu'il y a un problème dans
   votre code. *)
  
let verif_partie_1 () = 
  let plis = [{co = Coeur; va = Mineure 9}; {co = Pique; va = Valet}] in 
  assert (resultat {atout = Coeur; score = 20} plis = (false, -4));
  assert (resultat {atout = Pique; score = 20} plis = (true, 0));
  assert (resultat {atout = Carreau; score = 1} plis = (true, 1)) ;;


(***************************
******** Partie 2 **********
***************************)

(** TRI INSERTION **)


let rec insere x u =
  match u with
  |[]-> [x]
  |y::r -> if y>=x then x::y::r else y::insere x r ;;

let rec tri_insertion u =
  match u with
  |[]->[]
  |x::r -> insere x ( tri_insertion r) ;;
 
(** TRI FUSION **)

let rec split u =
  match u with
  |[]->([],[])
  |[x]-> ([x],[])
  |x::y::r -> let a,b = split r in (x::a, y::b) ;;

let rec merge (u,v) =
  match u,v with
  |[],[]->[]
  |a,[]->a
  |[],b->b
  |x::xs,y::ys -> if x<=y then x::( merge (xs,y::ys) ) else y::(merge (x::xs,ys)) ;;

let rec tri_fusion u=
  match u with
  |[]->[]
  |[x]->[x]
  |v-> let a,b = split v in merge ((tri_fusion a), (tri_fusion b)) ;;
  
(** TESTS ET MESURES **)

(* Liste aléatoire de n entiers entre 0 et 100 *)

let rec liste_alea n = if n = 0 then [] else Random.int 100 :: liste_alea (n - 1) ;;



(* range à la python (range a b = [a; a + 1; ... ; b - 1]) *)
let rec range deb fin = if deb >= fin then [] else deb :: range (deb + 1) fin ;;


(*
  Teste une fonction de tri sur divers listes (taille max 2048), 
  renvoie true SSI tous les tests ont réussi.
  Autrement dit, si vous appelez par exemple "teste tri_insertion",
  vous devez obtenir true (sinon, votre tri est faux).
*)
let teste f = 
  let cas_simples = [[]; [1]; [1; 2; 3]; [3; 2; 1]; [3; 1; 2]; [2; 2; 1]] in
  let aleas = 
    List.map (fun n -> liste_alea (1 lsl n)) (range 1 12) in
  let teste_liste m = (List.sort compare m = f m) in
  List.for_all teste_liste (cas_simples @ aleas) ;;

(***************************
******** Partie 3 **********
***************************)

(** GENERER LES COMBINAISONS **)


let rec combis l k = 
  let rec ajout x u =
    match u with
    |[]->[]
    |a::b -> (x::a)::(ajout x b) in if k = 0 then [[]] else
    match l,k with
    |[],_->[]
    |v,1 -> List.map (fun a->[a]) v
    |x::r,n -> (ajout x (combis r (n-1)))  @ combis r n ;;


	       

(* teste_combis combinaisons doit renvoyer () *)
let teste_combis f = 
  assert (List.sort compare (f [1; 2; 3; 4; 5] 3) = 
      [[1; 2; 3]; [1; 2; 4]; [1; 2; 5]; [1; 3; 4]; [1; 3; 5]; [1; 4; 5];
       [2; 3; 4]; [2; 3; 5]; [2; 4; 5]; [3; 4; 5]])


(** COMBINAISONS DANS L'ORDRE LEXICOGRAPHIQUE **)



let kieme_lexi u k = if k = 0 then [] else
  let rec proc p l a comp = match List.rev l with
  |[]-> failwith "liste vide"
  |x::r-> let crnt = List.length x in if comp+List.length l <= a then proc p (combis p (crnt +1)) a (comp+List.length l) else List.nth l (k-comp)
  in
  proc u (combis u 0) k 0;;

(* 
utop[39]> kieme_lexi (range 1 31) 987654321;;
- : int list = [4; 6; 9; 14; 15; 18; 20; 21; 22; 24; 26; 27; 28; 30]
 *)

(** COMBINAISONS DANS L'ORDRE DE L'ENONCE **)      

let rec deb_suivant p n = match p with
  |[]->[]
  |x::r -> if x<n then (x+1)::r else deb_suivant r (n-1) ;;


let prochaine p n = let k= List.length p in if k=n then None else let deb = deb_suivant (List.rev p) n in match deb, List.length deb with
  |[],_-> Some (range 1 (k+2))
  |x::r,q -> let fin = range (x+1) (x+1+k-q) in Some (List.rev_append deb fin) ;;


let kieme n k = 
  let rec aux n p comp = if comp = 0 then p else match p with | None -> failwith "error" | Some l ->  aux n (prochaine l n) (comp-1)
  in aux n (Some []) k ;;