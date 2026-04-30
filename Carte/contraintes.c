#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>
#include <string.h>
#include <assert.h>

char* contrainte_adjacence(char* r1, char* r2, int nb_couleurs) {
    // Taille estimée par clause : ~(R_[r1]_[i] & R_[r2]_[i]) & ...
    int taille = nb_couleurs * (strlen(r1) + strlen(r2) + 40) + 1;
    char* buffer = malloc(taille);
    char* tmp = malloc(taille);
    buffer[0] = '\0';

    for (int i = 0; i < nb_couleurs; i++) {
        sprintf(tmp, "~(R_[%s]_[%d] & R_[%s]_[%d])", r1, i, r2, i);
        if (i > 0) strcat(buffer, " & ");
        strcat(buffer, tmp);
    }

    free(tmp);
    return buffer;
}

char* toutes_contraintes_regions(char** regions, int n, int nb_couleurs) {
    // Estimation de la taille totale nécessaire
    int taille = 0;
    for (int i = 0; i < n; i++) {
        taille += nb_couleurs * (strlen(regions[i]) + 40) * 2;
    }

    char* contrainte = malloc(taille);
    contrainte[0] = '\0';

    for (int i = 0; i < n; i++) {
        char* au_moins = contrainte_au_moins_une_couleur(regions[i], nb_couleurs);
        char* au_plus  = contrainte_au_plus_une_couleur(regions[i], nb_couleurs);

        if (i > 0) strcat(contrainte, " & ");
        strcat(contrainte, au_moins);
        strcat(contrainte, " & ");
        strcat(contrainte, au_plus);
        free(au_moins);
        free(au_plus);
    }
    return contrainte;
}

char* toutes_contraintes_adjacences(int** adjacences, char** regions, int m, int nb_couleurs) {
    int taille = 0;
    for (int i = 0; i < m; i++) {
        taille += nb_couleurs * (strlen(regions[adjacences[i][0]]) 
                               + strlen(regions[adjacences[i][1]]) + 40);
    }

    char* contrainte = malloc(taille);
    contrainte[0] = '\0';

    for (int i = 0; i < m; i++) {
        char* cont = contrainte_adjacence(regions[adjacences[i][0]], regions[adjacences[i][1]], nb_couleurs);
        if (i > 0) strcat(contrainte, " & ");
        strcat(contrainte, cont);
        free(cont);
    }
    return contrainte;
}

void gen_formule_coloriage(char** regions, int n, int** adj, int m, int nb_couleurs, char* filename) {
    // Génération des deux blocs de contraintes
    char* contraintes_regions    = toutes_contraintes_regions(regions, n, nb_couleurs);
    char* contraintes_adjacences = toutes_contraintes_adjacences(adj, regions, m, nb_couleurs);

    // Assemblage de la formule complète
    int taille = strlen(contraintes_regions) + strlen(contraintes_adjacences) + 10;
    char* formule = malloc(taille);
    formule[0] = '\0';

    strcat(formule, contraintes_regions);
    strcat(formule, " & ");
    strcat(formule, contraintes_adjacences);

    // Écriture dans le fichier
    FILE* f = fopen(filename, "w");
    assert (f != NULL); 

    fprintf(f, "%s\n", formule);
    fclose(f);

    // Libération de la mémoire
    free(contraintes_regions);
    free(contraintes_adjacences);
    free(formule);
}