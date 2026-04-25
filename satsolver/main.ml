open Satsolver
open Quine

(* Convertit une valuation en liste des variables vraies *)
let rec true_vars (v : valuation) : string list =
  v
  |> List.filter (fun (x,b) ->
        b &&
        String.length x >= 8 &&
        x.[0] = 'X' && x.[1] = '_' &&
        x.[2] = '[' && x.[4] = ']' &&
        x.[5] = '_' &&
        x.[6] = '[' && x.[8] = ']'
     )
  |> List.map fst

(* Affiche un échiquier 8x8 à partir des variables vraies X_[i]_[j] *)
let print_chessboard vars =
  let board = Array.make_matrix 8 8 '.' in
  List.iter (fun v ->
    try
      let i = int_of_string (String.sub v 3 1) in
      let j = int_of_string (String.sub v 6 1) in
      board.(i).(j) <- 'Q'
    with _ -> ()
  ) vars;
  for i = 0 to 7 do
    for j = 0 to 7 do
      print_char board.(i).(j);
      print_char ' '
    done;
    print_newline ()
  done

let () =
  if Array.length Sys.argv < 2 then (
    print_endline "Usage: ./satsolver <fichier_formule>";
    exit 1
  );

  let filename = Sys.argv.(1) in
  let formule =
    try from_file filename
    with _ ->
      print_endline "Erreur : impossible de lire ou parser le fichier.";
      exit 1
  in

  match algorithme_quine formule with
  | None ->
      print_endline "La formule est insatisfiable."
  | Some v ->
      print_endline "La formule est satisfiable. Variables vraies :";
      print_true v;
      print_endline "\nÉchiquier correspondant :";
      print_chessboard (true_vars v)
