let rec random_liste_int n amp = if n=0 then [] else ( Random.int amp)::(random_liste_int (n-1) amp) ;;

let rec random_liste_float n amp = if n=0 then [] else ( Random.float amp)::(random_liste_float (n-1) amp) ;;