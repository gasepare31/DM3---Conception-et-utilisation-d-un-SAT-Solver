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
  print_string Sys.argv.(1); print_string "\n\n";
  let ic = open_in (Sys.argv.(1)) in 
  let res = input_line ic in 
  close_in ic; 
  print_string res


let _ = main() (* Exécution de la fonction main *)

