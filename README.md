# SAT Solver — DM3

Implémentation expérimentale d'un solveur SAT en OCaml, réalisée dans le cadre d'un devoir de conception et d'utilisation d'un solveur.

Le dépôt regroupe :
- un solveur SAT principal en OCaml ;
- une version naïve de résolution ;
- des exemples et problèmes de test ;
- un programme C autour de contraintes de coloriage de carte ;
- des réponses et fichiers de données associés au devoir.

## Organisation

- satsolver.ml : implémentation principale du solveur SAT.
- sat_sol_naif.ml : version naïve pour comparaison ou expérimentation.
- quine.ml : exemple lié aux formules auto-référentes et aux tests.
- problemes/ : problèmes, utilitaires et fichiers de test en C.
- Carte/ : exemple de coloriage de carte formulé comme problème de contraintes.
- Reponses-gaspard.txt : réponses personnelles associées au devoir.

La structure détaillée et les recommandations de rangement sont dans [docs/structure.md](docs/structure.md).

## Compilation

Pour explorer le solveur principal :

    ocamlc -o satsolver satsolver.ml
    ./satsolver

Si le programme attend des entrées ou arguments spécifiques, consulter son code source avant l'exécution.
