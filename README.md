pour execution dans dossier satsolver : (ordre important)

ocamlc -c quine.ml
ocamlc -c main.ml
ocamlc -o solver satsolver.cmo quine.cmo main.cmo

./solver formule_8_dames.txt
