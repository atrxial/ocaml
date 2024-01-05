(* 1 *)

let rec itere f x0 n = if n=0 then x0 else f (itere f x0 (n-1)) ;;

let rec itere2 f x0 n = if n=0 then x0 else f (f (itere f x0 (n-1))) ;;
(* 2*)

let floyd1 f x0 = if itere f x0 