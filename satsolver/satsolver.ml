let test () = 
  assert (1=1);
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
  print_string Sys.argv.(1); print_string "\n\n"


let _ = main() (* Exécution de la fonction main *)

