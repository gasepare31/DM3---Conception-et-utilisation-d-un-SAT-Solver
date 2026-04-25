open Satsolver

type valuation = (string * bool) list 

type sat_result =valuation option

let rec simpl_step (f: formule) : formule * bool =
  let res = ref false in 
  let rec simple_step (ff : formule) : formule =
    match ff with 
    | And(Top,g) | And(g,Top) -> res := true; g
    | And(Bot,g) | And(g,Bot) -> res := true; Bot
    | Or(Top,g) | Or(g,Top) -> res := true; Top
    | Or(Bot,g) | Or(g,Bot) -> res := true; g
    | Not(Not(g)) -> res := true; g
    | Not(Top) -> res := true; Bot
    | Not(Bot) -> res := true; Top
    |And(g,h) -> 
        let g1,_ = simpl_step g in
        let h1,_ = simpl_step h in
        And(g1,h1)
    |Or(g,h) -> 
        let g1,_ = simpl_step g in
        let h1,_ = simpl_step h in
        Or(g1,h1)
    |Not(g) -> 
        let g1,_ = simpl_step g in
        Not(g1)
    | g -> g 
  in 
  let g =simple_step f in 
  g, !res

(*simplifie la formule jusqu'à n'en plus pouvoir*)
let simpl_full (f:formule) : formule =
  let superf = ref f in 
  let hmm = ref true in 
  while !hmm= true do 
    let (sup1 , hmm1)= simpl_step !superf in 
    superf := sup1 ;
    hmm := hmm1
  done; 
  !superf

(*remplace tout les x dans la formule f par g*)
let rec subst (f: formule)(x: string)(g: formule): formule =
    match f with
    |Var(y) -> if y = x then g else Var(y)
    |Top -> Top
    |Bot -> Bot
    |And(a,b) -> And(subst a x g, subst b x g)
    |Or(a,b) -> Or(subst a x g, subst b x g)
    |Not(a) -> Not(subst a x g)

(*pour choisir une variable de f*)
let rec choix_var (f: formule): string option=
  match f with
  |Var(x) -> Some x
  |And(a,b) |Or(a,b) ->
       (match choix_var a with
       |Some x -> Some x
       |None -> choix_var b)
  |Not a -> choix_var a
  |Top |Bot -> None

(*choix de la variable la plus frequente : *)
let rec choix_var_count (f: formule): (string*int) option =
  match f with
  |Var x -> Some (x, 1)
  |Top |Bot -> None
  |Not a -> choix_var_count a
  |And(a,b) | Or(a,b) ->
      let ca = choix_var_count a in
      let cb = choix_var_count b in
      match ca, cb with
      |None, None -> None
      |Some x, None -> Some x
      |None, Some y -> Some y
      |Some (xa,na), Some (xb,nb) -> if na >= nb then Some (xa,na) else Some (xb,nb)

let choix_var_mieux (f: formule): string option =
  match choix_var_count f with
  |None -> None
  |Some (x,_) -> Some x

(* enlève les doublons en gardant la première occurrence *)
let rec remove_duplicates v =
  match v with
  |[] -> []
  |(x,b)::q ->
    let q2 = List.filter (fun (y,_) -> y <> x) q in (x,b) :: remove_duplicates q2

let sort_valuation v =
  List.sort (fun (x,_) (y,_) -> compare x y) v

(* vérifie si une formule est entièrement vraie *)
let rec is_true_formula f =
  match f with
  | Top -> true
  | Bot -> false
  | Var _ -> false
  | Not f1 -> not (is_true_formula f1)
  | And(f1,f2) -> is_true_formula f1 && is_true_formula f2
  | Or(f1,f2) -> is_true_formula f1 || is_true_formula f2

let rec algorithme_quine (f: formule): sat_result =
  let f = simpl_full f in
  if is_true_formula f then Some []
  else if f = Bot then None
  else
    let x = choix_var f in
    match x with
    |None -> None
    |Some y ->
    match algorithme_quine (simpl_full (subst f y Top)) with
    |Some v -> let v = (y,true)::v in Some (sort_valuation (remove_duplicates v))
    |None -> 
      match algorithme_quine (simpl_full (subst f y Bot)) with
      |Some v -> let v = (y,false)::v in Some (sort_valuation (remove_duplicates v))
      |None -> None              

(*affiche les variables vraies dans la valuation*)
let rec print_true (v : valuation) : unit =
  match v with
  |[] -> ()
  |(x,true) :: r ->
      print_endline x;
      print_true r
  |(_,false) :: r -> print_true r

let solve_affichage (f: formule): unit =
  match algorithme_quine f with
  |None -> print_endline "La formule est insatisfiable."
  |Some v -> 
    print_endline "La formule est satisfiable en assignant 1 aux variables suivantes et 0 aux autres:";
    print_true v
