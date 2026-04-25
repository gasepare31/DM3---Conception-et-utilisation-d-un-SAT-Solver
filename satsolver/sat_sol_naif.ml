type formule =
    |Var of string | Top | Bot
    |And of formule * formule 
    |Or of formule * formule 
    |Not of formule

type valuation = (string * bool) list

(*q12, interprete une formule dans une valuation donnée *)
let interpretation (f : formule)(v : valuation ): formule= 
    let rec int_bool (ff: formule)(vv :valuation) : bool =
      match ff with 
      | Top -> true 
      | Bot -> false 
      | Var(x) -> let res = List.assoc x vv in 
                  res 
      |Or(f1,f2) -> int_bool f1 vv ||  int_bool f2 vv 
      |And(f1,f2) -> int_bool f1 vv && int_bool f2 vv 
      |Not(f1) -> not (int_bool f1 vv)
    in 

    let res = int_bool f v in 
    if res then Top else Bot

(*q13, prend une liste de booléens represnetant x en binaire
et renvoie celle représentant x+1 *)
let add_one (l : bool list) : bool list =
  let l2 = List.rev l in 
  let rec add_one_since(ll: bool list)(acc : bool list) : bool list =
    match ll with 
    | [] -> true::acc
    | x::q -> if not x then List.rev (true::q) 
    else add_one_since q (false::acc)
  in 
  add_one_since l2 [] 

(*q14, renvoie valuation d'après *)
let valuation_next (v : valuation ) : valuation option = 
  let vars = List.map fst v in (*on sépare la double liste en simple liste*)
  let vals = List.map snd v in (*cette fonction est géniale*)

  let next_vals = add_one vals in

  if List.length next_vals > List.length vals then None
  else Some (List.combine vars next_vals) (*on réunit les deux listes*)
  
(*q15, renvoie valuation egale a false*)
let valuation_init (xlist : string list) : valuation =
  let rec vrai_fun (xmas : string list)(xxx : valuation) =
    match xmas with 
    | [] -> xxx
    | x::q -> vrai_fun q ((x,false)::xxx) 
  in 
  vrai_fun xlist []

type sat_result =valuation option

(*q16, sat solveur naif vrai de vrai final form*)
let salsolver_naif (f : formule) : sat_result =
  let lx = form_to_list f in (*fonction de gaspard*)
  let v = ref valuation_init lx in
  let res = ref None in  
  while (valuation_next !v <> None && !res =None) do 
    match !v with 
    |None -> ()
    |Some vv -> 
      if interpretation f vv =Top then 
        res := Some vv 
    else 
      v := valuation_next vv
  done;
  !res