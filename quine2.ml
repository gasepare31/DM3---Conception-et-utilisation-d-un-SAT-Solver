type formule =
    |Var of string | Top | Bot
    |And of formule * formule 
    |Or of formule * formule 
    |Not of formule

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

let rec algorithme_quine (f: formule): sat_result =
  let f = simpl_full f in
  match f with
  |Top -> Some []                        
  |Bot -> None
  |_ ->        
        let x = choix_var f in     
        match x with
        |None -> Some []                     
        |Some x ->
            let f1 = simpl_full (subst f x Top) in
            begin match algorithme_quine f1 with
            |Some v -> Some ((x, true) :: v)  
            |None ->
                let f2 = simpl_full (subst f x Bot) in
                begin match algorithme_quine f2 with
                |Some v -> Some ((x, false) :: v)
                |None -> None              
            end
        end
