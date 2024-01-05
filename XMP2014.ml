type arbre = 
  |E
  |N of arbre * int * arbre ;;



let minimum a = match a with
  |E -> 0
  |N (g,x,d) -> x ;;

let rec est_croissant a = match a with
  |E -> true
  |N (g,x,d) -> x <= minimum g && x <= minimum d && est_croissant g && est_croissant d ;;


let rec fusion a b = match a,b with
  |x,E |E,x -> x
  |N (g1,x1,d1), N (g2,x2,d2) when x1<=x2 -> N (fusion d1 b, x1,g1)
  |N (g1,x1,d1), N (g2,x2,d2) -> N (fusion d2 b, x2,g2) ;;




let ajoute a x = fusion a (N (E,x,E));;

let supprime_minimum a = match a with
  |E -> 0
  |N (g,x,d) -> fusion g d ;;


let ajouts_succ t = let a = ref E in
 for c = 0 to Array.length t -1 do
    a:= ajoute (!a ) t.(c)
  done ; !a ;;


(*pt clé, f1 et f2 alternent et pour 2**k, l'arbre est complet et ajouter une suite par la fonction ajoute va parcourir tous les E et finir par remplir avec des valeurs d'une suite croissante dont les termes de l'arbre par exemple f1 sont tous inférieurs au premier terme de la suite)
on prend les x_2**k+2p pour f1 par exemple et on conclut de même pour f2, on encadre donc la hauteur de l'arbre n =  2**(floor(log2(n))) + p, on a donc la hauteur avec l'hypothèse de récurrence que la hauteur d'un arbre de 1..2**k -1 est de k
*)

(* on crée un arbre qui indique si le nombre d'éléments etc .. puis on calcule la somme*)
let potentiel a =
  let rec aux a = match a with
  |E -> N (E,0,E)
  |N (g,e,d) -> let a,b = aux g, aux d in match a,b with
    |E,x | x,E -> x (* x car on va compter le nombre d'élements, exemple d'une tour*)
    |N (_,x,_) , N (_,y,_) -> N (a,x+y+1,b)
  in let rec eff a = match a with
  |E->0
  |N (g,x,d) -> let a,b=minimum g, minimum d in let m = eff d + eff g in if a<b then 1+ m else m
  in eff (aux a) ;;





  