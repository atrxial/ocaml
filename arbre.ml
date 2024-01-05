(*      8.1      *)



type ('a,'f) arbre = 
  |F of 'f
  |N of ('a,'f) arbre * 'a * ('a,'a) arbre ;;

let rec nb_feuille a = match a with
 |F _ -> 1
 |N (g,x,d) -> nb_feuille g + nb_feuille d ;;


let rec parcours_prof_pref a = match a with
  |F x -> [x]
  |N (g,x,d) -> x::( parcours_prof_pref g @ parcours_prof_pref d) ;;



let rec parcours_prof_inf a = match a with
  |F x -> [x]
  |N (g,x,d) ->  parcours_prof_pref g @ [x] @ parcours_prof_pref d ;;

  
let rec parcours_prof_pref a = match a with
|F x -> [x]
|N (g,x,d) -> x::( parcours_prof_pref g @ parcours_prof_pref d) @ [x] ;;