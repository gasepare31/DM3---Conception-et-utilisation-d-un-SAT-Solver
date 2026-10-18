# Organisation du dépôt

Le dépôt contient actuellement les sources OCaml, le sous-projet C, les données et les réponses du devoir. Une organisation cible non destructive serait :

    .
    ├── README.md
    ├── .gitignore
    ├── docs/                 notes et consignes de compilation
    ├── src/                  solveur OCaml
    ├── c/                    programmes C et headers
    ├── examples/             formules, cartes et problèmes
    └── answers/              réponses rendues

Les fichiers existants ne sont pas déplacés afin de ne pas casser les chemins ni modifier l'historique.

## Correspondance actuelle

- satsolver.ml, sat_sol_naif.ml, quine.ml : sources OCaml ;
- Carte/ et problemes/ : sous-projets et données associées ;
- Reponses-gaspard.txt : réponse personnelle ;
- .DS_Store et les exécutables : artefacts locaux à ne plus versionner.