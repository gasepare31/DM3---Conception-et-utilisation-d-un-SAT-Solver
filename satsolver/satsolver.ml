type formule =
	| Var of string
	| Top
	| Bot
	| And of formule * formule
	| Or of formule * formule
	| Not of formule

let implique (f1, f2) = Or(Not f1, f2)
let equivalence (f1, f2) = And(implique (f1, f2), implique (f2, f1))

(*** PARSER ***)

exception Erreur_syntaxe
exception Fichier_invalide

(* Symboles:
	'T' -> true
	'F' -> false
	'&' -> And
	'|' -> Or
	'~' -> Not
	'>' -> implication
	'=' -> equivalence
 *)

(* Détermine si c correspond à un opérateur binaire logique *)
let is_binop (c: char) : bool = match c with 
	| '&' |  '|' |  '>' |  '='  -> true
	| _ -> false 

(* Priorité de l'opérateur c. Permet de déterminer
	comment interpréter une formule sans parenthèses.
	Par exemple, "x&y|z" sera interprété comme "(x&y)|z"
	car & est plus prioritaire que | *)
let priority (c: char) : int = match c with
	| '&' -> 4
	| '|' -> 3
	| '=' -> 2
	| '>' -> 1
	| _ -> raise Erreur_syntaxe (* c n'est pas un opérateur *)

(* indice de l'opérateur le moins prioritaire parmis ceux
   qui ne sont pas entre parenthèses entre s.[i] et s.[j] 
   inclus *)
 let find_op_surface (s: string) (i: int) (j: int) : int =
 	(* 
 	   Renvoie l'indice de l'opérateur le moins prioritaire entre
 	   i et j, sachant que res est l'indice du meilleur opérateur
 	   entre i et k-1.
 	   paren_lvl: niveau d'imbrication actuel des parenthèses *)
 	let rec find_op_paren (k:int) (res:int) (paren_lvl: int) : int  =
 		if k=j+1 then res else
 		if s.[k] = '(' then find_op_paren (k+1) res (paren_lvl+1)
 		else if s.[k] = ')' then find_op_paren (k+1) res (paren_lvl-1) 

 		(* Le caractère lu est pris si l'on est hors des parenthèses,
 		   que le caractère est bien un opérateur, et qu'il est moins
 		   prioritaire que le meilleur résultat jusqu'ici *)
 		else if paren_lvl = 0 
 			 && is_binop s.[k] 
 			 && (res = -1 || priority s.[k] < priority s.[res]) 
 			 then find_op_paren (k+1) k (paren_lvl)
 		else find_op_paren (k+1) res (paren_lvl)
 	in find_op_paren i (-1) 0;;

(* Renvoie une formule construite à partir de la chaîne s.
   Lève une exception Erreur_syntaxe si la chaîne ne représente pas une formule valide. *)
let parse (s: string) : formule =
	let n = String.length s in
	(* construit une formule à partir de s[i..j] *)
	let rec parse_aux (i: int) (j:int) =
		if not (0 <= i && i < n && 0 <= j && j < n && i <= j ) then raise Erreur_syntaxe else
		if s.[i] = ' ' then parse_aux (i+1) j
		else if s.[j] = ' ' then parse_aux i (j-1)
		else let k = find_op_surface s i j in 
		if k = -1 then
			if s.[i] = '~' then 
				Not (parse_aux (i+1) j)
			else if s.[i] = '(' then
				begin 
					if (s.[j] != ')') then (print_int j; failwith "mauvais parenthésage") else
					parse_aux (i+1) (j-1)
				end
			else if (i = j && s.[i] = 'T') then Top
			else if (i = j && s.[i] = 'F') then Bot
			else let nom_variable = String.sub s i (j-i+1) in 
			if String.contains nom_variable ' ' then raise Erreur_syntaxe else Var nom_variable

		else match s.[k] with
			| '&' -> And(parse_aux i (k-1), parse_aux (k+1) j)
			| '|' -> Or(parse_aux i (k-1), parse_aux (k+1) j)
			| '=' -> equivalence(parse_aux i (k-1), parse_aux (k+1) j)
			| '>' -> implique(parse_aux i (k-1), parse_aux (k+1) j)
			| _ -> raise Erreur_syntaxe
	in parse_aux 0 (String.length s -1)

(* Renvoie une formule construire à partir du contenu du fichier fn.
   Lève une exception Erreur_syntaxe si le contenu du fichier n'est pas une formule valide.
   Lève une exception Sys_error(message_erreur) si le nom du fichier n'est pas valide. *)
let from_file (filename: string) : formule = 
	(* concatène toutes les lignes de f en une seule chaîne *)
	let rec read_lines f = 
		try 
			let next_line = input_line f in
			let s = read_lines f in
			next_line ^ s
		with 
			| End_of_file -> ""
	in
	let f = open_in filename in 
	let s = read_lines f in
	parse s

let rec compte_ops (f : formule) : int = 
  match f with
  | And (f1, f2) -> 1 + compte_ops f1 + compte_ops f2
  | Or (f1, f2) -> 1 + compte_ops f1 + compte_ops f2
  | Not f1 -> 1 + compte_ops f1
  | _ -> 0

let rec tri_et_doublon (l : 'a list) : bool = 
  match l with 
  | a :: b :: q -> if ((a<b) && (tri_et_doublon (b :: q) = true)) then true else false 
  | a :: [] -> true
  | [] -> true

let rec union (l1 : 'a list) (l2: 'a list) : 'a list = 
  match l1, l2 with
  | a :: q, b :: k -> if (a < b) then (a :: union q l2) else if (a = b) then b :: union q k else b :: union l1 k
  | [], l2 -> l2
  | l1, [] -> l1

let rec form_to_list (f : formule) : string list = 
  match f with
  | And (f1, f2) -> union (form_to_list f1) (form_to_list f2)
  | Or (f1, f2) -> union (form_to_list f1) (form_to_list f2)
  | Not f1 -> form_to_list f1
  | Var (x) -> [x]
  | _ -> []

let test_tri_et_doublon () = 
  assert (tri_et_doublon [1;2;3;4;5] = true);
  assert (tri_et_doublon [1;2;3;4;4] = false);
  assert (tri_et_doublon [1;2;1;4;5] = false)

let test_compte_ops() =
  assert (compte_ops( parse "A | B | C | (D | E)") = 4);
  assert (compte_ops( parse "A & B & C & (D & E)") = 4);
  assert (compte_ops( parse "~(A & B) & (~C) & (D & E)") = 6)

let test_parse () =
	assert (parse "a | (b & ~c)" = Or(Var "a", And(Var "b", Not (Var "c"))));
  assert (parse "~(a | (b & n))" = Not (Or (Var "a", And (Var "b", Var "n"))));
  assert (parse "a > b" = implique(Var "a", Var "b")); 
  assert (parse "a = b" = equivalence(Var "a", Var "b"));
  assert (parse "a | b & c" = Or(Var "a", And(Var "b", Var "c")));
  assert (parse "a | b & c = d > e" = implique( equivalence( Or(Var "a", And(Var "b", Var "c")), Var "d"), Var "e"));
  try (let _ = parse "a & & b" in failwith "erreur") with | Erreur_syntaxe -> () | _ -> failwith "Erreur_test";;
  try (let _ = parse "(a | b" in failwith "erreur") with | _ -> () ;
	print_string "Tests OK\n";;

let test_from_file () = 
  assert (from_file "tests/test1.txt" = Or (Var "a", And (Var "b", Not (Var "c"))));
  assert (from_file "tests/test2.txt" = And (Top, Bot));
  try (let _ = from_file "tests/test3.txt" in failwith "erreur" ) with _ -> () 

let test () = 
  assert (1=1);
  test_parse();
  test_from_file();
  print_string "Tous les tests ont réussi\n"

let main () = 
  test();
  if (Array.length Sys.argv) = 1 then 
    failwith "Aucun argument"
  else if (Sys.argv.(1) = "test") then 
    test()
  else
  print_int (Array.length Sys.argv); print_string "\n\n";
  print_string Sys.argv.(0); print_string "\n\n";
  print_string Sys.argv.(1); print_string "\n\n";
  let ic = open_in (Sys.argv.(1)) in 
  let res = input_line ic in 
  close_in ic; 
  print_string res


let _ = main() (* Exécution de la fonction main *)

