#ifndef CONTRAINTES_H
#define CONTRAINTES_H
#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>

char** lire_regions(char* filename, int* n);

int chercher_region(char** regions, int nb_regions, char* cible);

int** lire_adjacences(char* filename, int* m);

char* variable_couleur(char* region, int couleur);

char* au_plus_une(char** l, int n);

char* au_moins_une(char** l, int n);

char* contrainte_au_moins_une_couleur(char* region, int nb_couleurs);

char* contrainte_au_plus_une_couleur(char* region, int nb_couleurs);

char* contrainte_adjacence(char* r1, char* r2, int nb_couleurs);

char* toutes_contraintes_regions(char** regions, int n, int nb_couleurs);

char* toutes_contraintes_adjacences(int** adjacences, char** regions, int m, int nb_couleurs);

void gen_formule_coloriage(char** regions, int n, int** adj, int m, int nb_couleurs, char* filename);

void afficher_coloriage(char* fichier_valuation, char** regions, int n);

#endif