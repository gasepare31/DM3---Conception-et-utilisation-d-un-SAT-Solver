#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <assert.h>
#include "contraintes.h"

int main(int argc, char* argv[]) {
    if (argc < 4) {
        printf("Usage : %s nb_couleurs region_adj.txt formule.txt\n", argv[0]);
        return 1;
    }

    int nb_couleurs = atoi(argv[1]);
    char* fichier_regions = argv[2];
    char* fichier_formule = argv[3];

    int n, m;

    // Lecture des régions et adjacences
    char** regions = lire_regions(fichier_regions, &n);
    int** adj = lire_adjacences(fichier_regions, &m);

    // Génération de la formule
    gen_formule_coloriage(regions, n, adj, m, nb_couleurs, fichier_formule);

    printf("Formule générée dans %s\n", fichier_formule);
    
    // Appel du solver OCaml
    char cmd[512];
    sprintf(cmd, "./solver %s > valuation.txt", fichier_formule);
    system(cmd);

    // Affichage du coloriage
    afficher_coloriage("valuation.txt", regions, n);

    return 0;
}