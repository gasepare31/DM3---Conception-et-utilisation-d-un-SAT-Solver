open Satsolver
open Quine

let () =
  if Array.length Sys.argv < 2 then (
    print_endline "Usage: ./solver <fichier_formule>";
    exit 1
  );

  let filename = Sys.argv.(1) in
  let formule =
    try from_file filename
    with _ ->
      print_endline "Erreur : impossible de lire le fichier.";
      exit 1
  in

  solve_affichage formule