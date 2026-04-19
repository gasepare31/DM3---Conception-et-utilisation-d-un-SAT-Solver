type formule =
    |Var of string | Top | Bot
    |And of formule * formule 
    |Or of formule * formule 
    |Not of formule

type valuation = (string * bool) list 

type sat_result =valuation option

(*renvoie un bouleen si une modification a ete faite avec quine*)
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


