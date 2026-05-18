

(*===============================DEFINITON DES TYPES==============================================*)


type formule =
	| Var of string
	| Top
	| Bot
	| And of formule * formule
	| Or of formule * formule
	| Not of formule

let implique (f1, f2) = Or(Not f1, f2)
let equivalence (f1, f2) = And(implique (f1, f2), implique (f2, f1))


type valuation = (string*bool) list

type sat_result = valuation option


(*===========================LECTURE DES FICHIERS=================================================*)

(*** PARSER ***)

exception Erreur_syntaxe
exception Fichier_invalide

(* Symboles:
	'T' -> Top
	'F' -> Bot
	'&' -> And
	'|' -> Or
	'~' -> Not
	'>' -> implique
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

(* Renvoie le contenu du fichier fn sous forme de string.
Le fichier ne doit contenir qu'une seule ligne *)
let read_file (fn: string): string = 
  let ic = open_in fn in 
  let res = input_line ic in 
  close_in ic; res

let test_parse () =
  (*Base*)
  assert(parse "T" = Top);
  assert(parse "F" = Bot);
  assert(parse "a & b" = And(Var "a", Var "b"));
  assert(parse "a | b" = Or(Var "a", Var "b"));
  assert(parse "~a" = Not(Var "a"));
  assert(parse "a > b" = implique(Var "a", Var "b"));
  assert(parse "a = b" = equivalence(Var "a", Var "b"));
  assert(parse " < " = Var("<"));
  (*Plus complexe*)
  assert(parse "a | (b & ~c)" = Or(Var "a", And(Var "b", Not (Var "c"))));
  assert(parse "a & ~b" = And(Var "a",Not(Var "b")));
  assert(parse "a > (b & c)" = implique(Var "a",And(Var "b",Var "c")));
  assert(parse "T = ~F" = equivalence(Top,Not(Bot)));
  (*Erreurs*)
  assert( try let _ = parse "a|" in false with 
  Erreur_syntaxe -> true);
  assert( try let _ = parse "a&&b" in false with 
  Erreur_syntaxe -> true);
  assert( try let _ = parse " > " in false with 
  Erreur_syntaxe -> true);
  (*assert(try let _ = parse "(a > b" in false with 
  Failure _ -> true);*) (*Tests de mauvais parenthésage. En commentaire car affiche indice*)
  assert(try let _ = parse "a) > b" in false with 
  Erreur_syntaxe -> true)


let test_from_file() = 
  assert(from_file "tests/formule1.txt" = And(Var "a", Not(Var "b")));
  assert(from_file "tests/formule2.txt" = implique(And(Var "a",Var "b"), Var "a"));
  assert(try let _ = from_file "fichier_inexistant" in false with 
  Sys_error _ -> true);
  assert(try let _ = from_file "tests/formule_fausse1.txt" in false with 
  Erreur_syntaxe -> true)
  (*; assert(try let _ = from_file "tests/formule_fausse2.txt" in false with 
  Failure _ -> true)*) (*Tests de mauvais parenthésage. En commentaire car affiche indice*)


(*==============================================================================================*)

(*========================================PARTIE 1==============================================*)

(*==============================================================================================*)


(*=================================MANIPULATION DE FORMULES LOGIQUES============================*)


(*compte_ops f renvoie le nombre d'opérateurs utilisés dans f*)
let rec compte_ops (f: formule): int = 
  match f with 
  | Top -> 0
  | Bot -> 0
  | Var _ -> 0 
  | And(a,b) -> 1 + compte_ops a + compte_ops b
  | Or(a,b) -> 1 + compte_ops a + compte_ops b
  | Not a -> 1 + compte_ops a

(*Détermine si l est triée et sans doublons*)
let rec liste_triee (l: 'a list): bool = 
  match l with
  | [] -> true 
  | [x] -> true
  | x::y::q -> if x<y then liste_triee (y::q) else false

(*Renvoie une liste triée et sans doublons, formée des éléments de l1 et l2.
  Précondition : l1 et l2 sont triées et sans doublons*)
let rec union (l1: 'a list)(l2: 'a list): 'a list = 
  match l1,l2 with 
  | [] , l2 -> l2 
  | l1 , [] -> l1 
  | x::q1 , y::q2 -> if x<y then x::(union q1 l2) else 
    if x>y then y::(union l1 q2) else x::(union q1 q2)

(*Renvoie la liste des variables dans f, sans doublons*)
let rec liste_var (f: formule): 'a list =
    match f with 
    | Var x ->[x]
    | Top  | Bot-> []
    | And(a,b)  | Or(a,b)-> union (liste_var a)(liste_var b)
    | Not a -> liste_var a

(*renvoie la tete de la liste l*)
let tete (l: 'a list): 'a =
  match l with 
  | [] -> failwith "liste vide"
  | x::q -> x



(*==================================SAT SOLVER NAIF==============================================*)


(* interprète la formule f dans la valuation sigma *)
let rec interp (sigma : valuation) (f : formule) : bool = 
  match f with
  | Top -> true
  | Bot -> false
  | Var v -> List.assoc v sigma
  | And(f1, f2) -> interp sigma f1 && interp sigma f2
  | Or(f1,f2) -> interp sigma f1 || interp sigma f2
  | Not f1 -> not (interp sigma f1)

(* à partir d'une liste l de booléen représentant un nombre x en binaire, renvoie
   x + 1 sous la forme d'une liste de booléen *)
let add_one (l : bool list) : bool list =
  let rec add_one_rev (l : bool list) : bool list =
    match l  with
    | [] -> []
    | x::q -> if x=false then true::q 
        else false :: add_one_rev q
  in List.rev (add_one_rev (List.rev l))
  
  
(* à partir d'une valuation sigma , renvoie la valuation suivante, et None si sigma est la valuation maximale *)
let rec valuation_next (sigma : valuation) : valuation option =
  (* récupère dans la valuation sigma la liste des booléens correspondant aux valeurs des variables*)
  let rec recup_val (sigma : valuation) : bool list =
    match sigma with
    | [] -> []
    | (_,v) :: q -> v:: recup_val q
  in
  let new_val = add_one (recup_val sigma) in
  (* vérifie si new_val n'est composé que de false ( càd sigma est la valuation maximale)*)
  let rec sigmax (list_val : bool list) :  bool =
    match list_val with
    | [] -> true
    | x::q -> if x=true then false else sigmax q
  in let b=sigmax new_val in
  if b=true then None
  else 
  (* associe les nouvelles valeurs avec les variables correspondantes dans une nouvelle valuation*)
  let rec assoc_val (sigma : valuation) (liste_val : bool list) : valuation =
    match sigma, liste_val with
    | [], []->  []
    | (s,v)::r, x::q -> (s,x) :: assoc_val r q
    | _,_ -> failwith "problème"
  in Some(assoc_val sigma new_val)

(* renvoie la valuation où toutes les variables de la liste liste_variable sont à false *)
let rec valuation_init (liste_variable : string list) : valuation =
  match liste_variable with
  | [] -> []
  | x::q -> (x, false) :: valuation_init q

(* sat-solver naïf, à partir d'une formule renvoie une valuation qui la satisfait, None si il n'en existe aucune *)
let satsolver_naif (f : formule) : sat_result =
  let sigma=valuation_init (liste_var f) in
  let rec test_valuation (valu : valuation) : sat_result =
    if interp valu f = true then Some valu else 
    match valuation_next valu with
    | None -> None
    | Some valnext -> test_valuation valnext
  in test_valuation sigma




(*===============================ALGORITHME DE QUINE=============================================*)


(*======FONCTIONS AUXILIAIRES DE L'ALGO DE QUINE=======*)


(*Effectue une étape de simplification d'une formule
   Entree: Formule f à simplifier
   Sortie: f',b Formule*bool avec b indiquant si une 
   simplification non récursive a été effectuée*)

   let rec simpl_step(f: formule): formule*bool=
   match f with
   |And(Top,f')|And(f',Top)-> f',true
   |And(Bot,f')|And(f',Bot)-> Bot,true
   |Or(Top,f') |Or(f',Top)-> Top,true
   |Or(Bot,f') |Or(f',Bot)-> f',true
   |Not(Not(f'))-> f',true
   |Not(Top)-> Bot,true
   |Not(Bot)-> Top,true
 
   |And(f1,f2)-> let f1',b1 = simpl_step f1 in
                 let f2',b2 = simpl_step f2 in
     And(f1',f2'),(b1||b2)
   |Or(f1,f2)-> let f1',b1 = simpl_step f1 in
                 let f2',b2 = simpl_step f2 in
     Or(f1',f2'),(b1||b2)
   |Not(f1)-> let f1',b1 = simpl_step f1 in
      Not(f1'),b1
 
 
   |_-> f,false
 
 (*Effectue plusieurs itérations de simplification jusqu'à trouver un point fixe
    Entree: Formule f à simplifier 
    Sortie: Formule f' entièrement simplifiée*)
 let rec simpl_full(f: formule): formule=
   let f',b  = simpl_step f in
   if b then simpl_full f' 
   else f'
   
 
  
 
 (*Renvoie la formule ou toutes les occurences de x une variable
    ont été remplacées par la formule g
    Entree: Formule f à modifier, x une variable, g une formule
    Sortie: Formule modifiée *)
 let rec subst(f: formule)(x: string)(g: formule):formule=
   match f with
   | Var(y)-> if y = x then g 
             else Var(y)
   | Top -> Top
   | Bot -> Bot
   | And(f1,f2) -> And(subst f1 x g, subst f2 x g)
   | Or(f1,f2) -> Or(subst f1 x g, subst f2 x g)
   | Not(f1) -> Not(subst f1 x g)
 
 
 
 (*======STRATEGIE DU CHOIX DE VARIABLES + TOP OU BOT ======*)
 
 
 (*Choix de la variable*)
 
 (*Option 1: Tete de liste/ Aléatoire*)
 
 let choix_tete(f: formule): string=
   tete (liste_var f) 
 
 
 (*Option 2: Variable la plus fréquente*)
 
 
 (*Modifie en place la table de hashage h, en comptant le nombre
    d'occurences des variables*)
 let rec parcours_occ(f: formule)(h: (string, int)Hashtbl.t): unit=
   match f with
   | Var x ->
     let c = try Hashtbl.find h x with 
       |Not_found -> 0 
     in Hashtbl.replace h x (c+1)
   | Top | Bot -> ()
   | And(f1,f2) | Or(f1,f2) -> parcours_occ f1 h; parcours_occ f2 h
   | Not(f1) -> parcours_occ f1 h
 
 
 (*Renvoie une table de hashage comprenant le nombre d'occurence
    de chaque variable de la formule f*)
 let compte_occ(f: formule):(string, int)Hashtbl.t=
   let h = Hashtbl.create (List.length (liste_var f)) in
   parcours_occ f h; h
 
 (*Renvoie la clé de la valeur maximale d'une table de hashage h*)
 let hashtbl_key_max(h: (string, int)Hashtbl.t):string =
   let c_max = ref 0 in
   let x_max = ref "" in
   Hashtbl.iter(fun x n -> 
     if n > !c_max then begin
       c_max := n;
       x_max := x
     end
   ) h; 
   !x_max
 
 
 (*Renvoie la variable la plus fréquente dans une formule*)
 let choix_freq(f: formule):string =
   hashtbl_key_max  (compte_occ f)
 
 (*Option 3: On optimise les rares cas avec des And(x,_) en assignant la valeur de x souhaitée*)
 
 (*Renvoie Some (x,b) si x apparaît seule dans un And 
    None s'il n y a pas de variable de la sorte *)
 
 let rec var_forcee (f: formule):(string * bool)option=
   match f with
   | And(Var (x), _)| And(_, Var (x))-> Some (x, true)
   | And(Not(Var (x)), _) | And(_, Not(Var (x)))-> Some (x, false)
   | And(f1,f2)-> 
     begin
       match var_forcee f1 with
      | Some (x,b) -> Some (x,b)
      | None -> var_forcee f2
     end
 
   | _ -> None
 
 
 (*Choix Top ou Bottom*)
 
 (*Choix 1: Toujours Top d'abord ie l'algo naif*)
 
 (*Choix 2: Top si majorité (ou égalité) de OU, 
    Bot si majorité de ET*)
 
 
 (*Renvoie le nombre de OU et de ET utilisés dans f*)
 let compte_ou_et (f: formule): int*int = 
 
   (*Renvoie le nombre de OU et de ET dans la formule f', ajouté aux accumulateurs*)
   let rec compte_ou_et_plus (f': formule)(acc1: int)(acc2: int): int* int=
     match f' with 
     | Top | Bot | Var _ -> acc1, acc2
     | And(f1,f2) -> let a1',a2' = compte_ou_et_plus f1 acc1 (acc2+1) in
                     compte_ou_et_plus f2 a1' a2'
 
 
     | Or(f1,f2) -> let a1',a2' = compte_ou_et_plus f1 (acc1+1) acc2 in
                    compte_ou_et_plus f2 a1' a2'
     | Not(f1) -> compte_ou_et_plus f1 acc1 acc2
 
   in compte_ou_et_plus f 0 0
 
 
 (*Renvoie true s'il y a plus de OU que de ET, false sinon*)
 let choix_top_bot(f: formule): bool=
   let o,e = compte_ou_et f in
   if o>= e then true
   else false
 
 
 
 
 
 
 (*========ALGO DE QUINE FINAL========*)
 
 (*Applique le principe de l'algortihme de Quine naif
    Entree: Formule f à simplifer
    Sortie: Some val s'il existe un valuation satisfaisant f
             None sinon *)
 let rec quine_naif(f: formule):sat_result=
   match f with
   | Top -> Some []
   | Bot -> None
   | _ -> let x = choix_tete f in
     let f_top= subst f x Top in
     let f_top_simpl = simpl_full f_top in
     let res_top = quine_naif f_top_simpl in
     begin
     match res_top with
     | Some(sigma)-> Some ((x,true)::sigma)
     | None -> 
       let f_bot = subst f x Bot in
       let f_bot_simpl = simpl_full f_bot in
       let res_bot = quine_naif f_bot_simpl in 
       begin
       match res_bot with
       | Some(sigma) -> Some ((x,false)::sigma)
       | None -> None
       end
     end
 
 
 (*Applique le principe de l'algortihme de Quine optimisé v1
    Entree: Formule f à simplifer
    Sortie: Some val s'il existe un valuation satisfaisant f
             None sinon *)
 
 let rec quine_optimise_v1(f: formule):sat_result=
   match f with
   | Top -> Some []
   | Bot -> None
   | _ -> let x = choix_freq f in
     
     if choix_top_bot f then
       let f_top= subst f x Top in
       let f_top_simpl = simpl_full f_top in
       let res_top = quine_optimise_v1 f_top_simpl in
       begin
       match res_top with
       | Some(sigma)-> Some ((x,true)::sigma)
       | None -> 
         let f_bot = subst f x Bot in
         let f_bot_simpl = simpl_full f_bot in
         let res_bot = quine_optimise_v1 f_bot_simpl in 
         begin
         match res_bot with
         | Some(sigma) -> Some ((x,false)::sigma)
         | None -> None
         end
       end
     
     else 
       let f_bot= subst f x Bot in
       let f_bot_simpl = simpl_full f_bot in
       let res_bot = quine_optimise_v1 f_bot_simpl in
       begin
       match res_bot with
       | Some(sigma)-> Some ((x,false)::sigma)
       | None -> 
         let f_top = subst f x Top in
         let f_top_simpl = simpl_full f_top in
         let res_top = quine_optimise_v1 f_top_simpl in 
         begin
         match res_top with
         | Some(sigma) -> Some ((x,true)::sigma)
         | None -> None
         end
       end
 
 
 (*Applique le principe de l'algortihme de Quine optimisé v2
    Entree: Formule f à simplifer
    Sortie: Some val s'il existe un valuation satisfaisant f
             None sinon *)
 let rec quine_optimise_v2(f: formule):sat_result=
   match f with
   | Top -> Some []
   | Bot -> None
   | _ -> let x, valeur = match var_forcee f with
         | Some (x, b) -> x, b  
         | None -> choix_freq f, choix_top_bot f
       in 
     if valeur then
       let f_top= subst f x Top in
       let f_top_simpl = simpl_full f_top in
       let res_top = quine_optimise_v2 f_top_simpl in
       begin
       match res_top with
       | Some(sigma)-> Some ((x,true)::sigma)
       | None -> 
         let f_bot = subst f x Bot in
         let f_bot_simpl = simpl_full f_bot in
         let res_bot = quine_optimise_v2 f_bot_simpl in 
         begin
         match res_bot with
         | Some(sigma) -> Some ((x,false)::sigma)
         | None -> None
         end
       end
     else 
 
       let f_bot = subst f x Bot in
       let f_bot_simpl = simpl_full f_bot in
       let res_bot = quine_optimise_v2 f_bot_simpl in 
       begin
       match res_bot with
       | Some(sigma)-> Some ((x,false)::sigma)
       | None -> 
         let f_top= subst f x Top in
         let f_top_simpl = simpl_full f_top in
         let res_top = quine_optimise_v2 f_top_simpl in
         begin
         match res_top with
         | Some(sigma) -> Some ((x,true)::sigma)
         | None -> None
         end
       end
 
 
 
 
 (*=========TEST COMPARAISON NAIF ET OPTIMISE===========*)
 
 (*Renvoie le temps d'exécution d'une fonction g de paramètres x en secondes *)
 let tmps_exe (g: 'a -> 'b) (x: 'a) : float =
   let t0 = Sys.time () in
   let _  = g x in
   Sys.time () -. t0
 
 (*Affiche le temps d'execution de l'algo naif d=et de l'algo optimisé pour une meme formule f *)
 let tmps_affiche(f: formule) : unit =
   let t_naif = tmps_exe quine_naif    f in
   let t_opt1  = tmps_exe quine_optimise_v1 f in
   let t_opt2 = tmps_exe quine_optimise_v2 f in 
   Printf.printf "algo_naif: %.6fs   algo_optimise_v1: %.6fs  algo_optimise_v2: %.6fs\n" t_naif t_opt1 t_opt2
 
 let test_comparaison()=
 
   let f1 = And(Var "x0", And(Var "x1", And(Var "x2", And(Var "x3",
           And(Var "x4", And(Var "x5", And(Var "x6", Var "x7"))))))) in
 
   let f2 =
     And(Var "x0",
     And(Or(Not(Var "x0"), Var "x1"),
     And(Or(Not(Var "x1"), Var "x2"),
     And(Or(Not(Var "x2"), Var "x3"),
     And(Or(Not(Var "x3"), Var "x4"),
     And(Or(Not(Var "x4"), Var "x5"),
         Not(Var "x5"))))))) in
   
   let f3 = And (And(Var "x0",
   And(Or(Not(Var "x0"), Var "x1"),
   And(Or(Not(Var "x1"), Var "x2"),
   And(Or(Not(Var "x2"), Var "x3"),
   And(Or(Not(Var "x3"), Var "x4"),
   And(Or(Not(Var "x4"), Var "x5"),
       Not(Var "x5"))))))),And(Var "x0",
       And(Or(Not(Var "x0"), Var "x1"),
       And(Or(Not(Var "x1"), Var "x2"),
       And(Or(Not(Var "x2"), Var "x3"),
       And(Or(Not(Var "x3"), Var "x4"),
       And(Or(Not(Var "x4"), Var "x5"),
           Not(Var "x5")))))))  ) in
     
   tmps_affiche f1; tmps_affiche f2; tmps_affiche f3
 
   

   


(*==================================BATTERIE DE TEST GENERALE/PARTIE1===========================*)


(*** TESTS/ MANIPULATION DE FORMULES LOGIQUES ***)


let test_compte_ops () = 
  assert(compte_ops (parse "x| (y& ~z)") = 3);
  assert(compte_ops (parse "~(x|(x&~y)|y)") = 5);
  assert(compte_ops (parse "~(x|(x&~y)|y)") = 5);
  assert(compte_ops (parse "x>y") = 2)

let test_liste_triee () = 
  assert(liste_triee [1;2;3] = true);
  assert(liste_triee [1;2;3;4] = true);
  assert(liste_triee [1;2;4;3] = false);
  assert(liste_triee [1;2;2;3] = false)

let test_union() = 
  assert(union [1;2;3] [4;5;6] = [1;2;3;4;5;6]);
  assert(union [1;2] [0] = [0;1;2]);
  assert(union [4;5;6;7] [4;6;9] = [4;5;6;7;9])

let test_liste_var () = 
  assert(liste_var (parse "x| (y& ~z)") = ["x";"y";"z"]);
  assert(liste_var (parse "~(x|(x&~y)|y)") = ["x";"y"])

let test_1_manipulation_formules_logiques() = 
  test_compte_ops();
  test_liste_triee();
  test_union();
  test_liste_var()

(*** TESTS SAT SOLVER NAIF ***)

  
let test_add_one()=
  assert(add_one [true; false ; false; true; true]=[true; false; true; false; false]);
  assert(add_one [true; false; false; true; false] = [true; false; false; true; true])

let test_valuation_next()=
  assert(valuation_next [("X", true); ("Y", true); ("Z", true)]=None);
  assert(valuation_next [("X", false); ("Y", true); ("Z", true)]=Some[("X", true); 
                          ("Y", false); ("Z", false)])

let test_valuation_init()=
  assert(valuation_init ["X";"Y";"Z";"I"]=[("X", false); ("Y", false); ("Z", false); ("I", false)]);
  assert(valuation_init [] = [])

let test_satsolver_naif()=
  assert(satsolver_naif Top = Some []);
  assert(satsolver_naif (And(Var("X"),Var("Y")))= Some [("X", true); ("Y", true)]);
  assert(satsolver_naif (And(Var "X", Not(Var "X")))=None);
  assert(satsolver_naif(And(And(Var "X", Not(Var "X")), Var "Y"))=None);
  assert(satsolver_naif(And(Var "X", Not(Var "Y")))=Some [("X",true); ("Y", false)]);
  assert(satsolver_naif(And(implique(Var "X", Var "Y"), implique(Var "Y", Var "Z")))=
                          Some [("X", false); ("Y", false); ("Z", false)]);
  assert(satsolver_naif (And(equivalence(Var "X", Var "Y"), Var "Z"))=
                          Some [("X",false);("Y",false); ("Z", true)])


let test_2_sat_solver_naif()=
  test_add_one();
  test_valuation_next();
  test_valuation_init();
  test_satsolver_naif()



(*** TESTS ALGO DE QUINE ***)


let test_simpl_step () =
  assert (simpl_step (Or(Bot, Var "x")) = (Var "x", true));
  assert (simpl_step (Not Top) = (Bot, true));
  assert (simpl_step Top = (Top,false));
  assert (simpl_step (And(Not(Not(Var "x")), Var "y")) = (And(Var "x", Var "y"),true))

let test_simpl_full () =
  assert (simpl_full Bot = Bot);
  assert (simpl_full (And(Or(Bot,Top), Not Bot)) = Top);
  assert (simpl_full (Not(Not(Not(Not Top)))) = Top)


let test_subst () =
  assert (subst (Var "x") "x" Top = Top);
  assert (subst Bot "x" Top = Bot);
  assert (subst (And(Or(Var "x",Var "y"), Not(Var "x"))) "x" Top = And(Or(Top,Var "y"), Not Top))


let test_choix_tete () =
  assert (choix_tete (Var "x") = "x");
  assert (choix_tete (Or(Var "x",Var "y")) = "x");
  assert (choix_tete (And(Var "a", Or(Var "b",Var "a"))) = "a")


let test_compte_occ () =
  let h1 = compte_occ (And(Var "x", Or(Var "x", Var "y"))) in
  assert (Hashtbl.find h1 "x" = 2);
  assert (Hashtbl.find h1 "y" = 1)


let test_choix_freq () =
  assert (choix_freq (And(Var "x", Or(Var "x", Var "y"))) = "x");
  assert (choix_freq (Var "z") = "z")

let test_var_forcee () =
  assert (var_forcee (And(Var "x", Var "y")) = Some ("x", true));
  assert (var_forcee (And(Not(Var "x"),Var "y")) = Some ("y",true));
  assert (var_forcee (Var "x") = None);
  assert (var_forcee (And(Or(Var "x",Var "y"),Var "z")) = Some ("z", true))

let test_compte_ou_et () =
  assert (compte_ou_et (Var "x") = (0, 0));
  assert (compte_ou_et (And(Var "x",Var "y")) = (0, 1));
  assert (compte_ou_et (And(Or(Var "x", Var "y"), Or(Var "a",Var "b"))) = (2, 1))


let test_choix_top_bot () =
  assert (choix_top_bot (Var "x") = true);
  assert (choix_top_bot (And(Var "x",Var "y")) = false);
  assert (choix_top_bot (And(Or(Var "x", Var "y"), Or(Var "a",Var "b"))) = true)


let test_quine_naif () =
  assert (quine_naif Top = Some []);
  assert (quine_naif (And(Var "x",Not(Var "x"))) = None);
  assert (quine_naif (Or(Var "x",Not(Var "x")))  <> None)


let test_quine_optimise_v1 () =
  assert (quine_optimise_v1 Top = Some []);
  assert (quine_optimise_v1 (And(Var "x", Not(Var "x"))) = None);
  assert (quine_optimise_v1 (Or(Var "x",Not(Var "x")))  <> None)


let test_quine_optimise_v2 () =
  assert (quine_optimise_v2 Top = Some []);
  assert (quine_optimise_v2 (And(Var "x", Not(Var "x"))) = None);
  assert (quine_optimise_v2 (Or(Var "x",Not(Var "x")))  <> None)

    
let test_3_algo_de_quine()=
  test_simpl_step(); 
  test_simpl_full(); 
  test_subst(); 
  test_choix_tete(); 
  test_compte_occ();
  test_choix_freq();
  test_var_forcee();
  test_compte_ou_et(); 
  test_choix_top_bot(); 
  test_quine_naif(); 
  test_quine_optimise_v1();
  test_quine_optimise_v2()


(**** GENERAL ***)

let test()=
  print_string "Vérification des tests... \n";
  test_1_manipulation_formules_logiques();
  test_2_sat_solver_naif();
  test_3_algo_de_quine();
  print_string "Tous les tests ont réussi ! \n"


(*===========================MAIN===============================================================*)


(*Affiche toutes les variables mise à true dans une valuation
   Entree: v la valuation
   Sortie: () *)
   let rec print_true(l: valuation):unit=
   match l with
   |[]-> ()
   |(x,b)::q -> if b then print_string (x^"\n");
                 print_true q
 

  let main() = 
  if (Array.length Sys.argv) < 2 then failwith "Manque argument" 
  else begin
    if Sys.argv.(1) = "test" then test() 
    else begin
    let f = parse (read_file Sys.argv.(1)) in
    match quine_optimise_v2 f with
    |None -> print_string "La formule est insatisfiable \n";
    |Some l -> print_string "La formule est satisfiable en assignant 1 aux variables suivantes et
                                0 aux autres \n"; print_true l 
    end
  end
 

let _ = main () (*exécution de la fonction main*)

