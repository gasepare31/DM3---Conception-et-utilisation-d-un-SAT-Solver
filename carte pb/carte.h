#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>
#include <string.h>
#include <assert.h>

/*la liste des régions est au format NOM:VOISIN1,VOISIN2 donc on
lit jusqu'au double point, le n sert a stocker la taille */
char** lire_regions(char* filename, int* n);

/* Cherche le nom `cible` dans `regions` et renvoie son indice,
 -1 si absent */
int chercher_region(char** regions, int nb_regions, char* cible) ;

/*renvoie la liste des couples (a,b) qui sont a coté (en utlisant 
leur numéro plutot que leur nom), avec a<b . m est nb de cpl*/
int** lire_adjacences(char* filename, int* m);

/*génère "R_'region'_'couleur'" avec la couleur un chiffre"*/
char* variable_couleur(char* region, int couleur);

/*au plus une (j'ai pris les fonctions d'avant de guaspar) */ 
char* au_plus_une(char** l, int n);

/*au moins une*/
char* au_moins_une(char** l, int n);

/*Pour la région 'region' ça génère d'abord
lz tableau avec ttes les couleurs puis la clause qui en select au 
moins une */
char* contrainte_au_moins_une_couleur(char* region, int nb_couleurs);

/*Pour la région 'region' ça génère d'abord
lz tableau avec ttes les couleurs puis la clause qui en select au 
plus une */
char* contrainte_au_plus_une_couleur(char* region, int nb_couleurs);
