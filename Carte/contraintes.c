#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>
#include <string.h>
#include <assert.h>
#include "contraintes.h"

/*la liste des régions est au format NOM:VOISIN1,VOISIN2 donc on
lit jusqu'au double point, le n sert a stocker la taille */
char** lire_regions(char* filename, int* n){
    char** liste = malloc (200*sizeof(char*));
    FILE* f = fopen(filename, "r");
    *n=0;
    int reg;
    char nom[100];
    int taille =0;
    while ((reg =fgetc(f))!= EOF) {
        if (reg== ':'){ //c la fin du nom
            nom[taille]= '\0';
            liste[*n]=malloc(taille + 1);
            strcpy(liste[*n], nom);
            (*n)++;
            taille=0;
            //après on ignore les voisins
            while ((reg =fgetc(f))!= EOF && reg != '\n');
        }
        else { //on stock le nom de la région 
            nom[taille]=reg;
            taille++;
        }
    }
    fclose(f);
    return liste ;
}

/* Cherche le nom `cible` dans `regions` et renvoie son indice,
 -1 si absent */
int chercher_region(char** regions, int nb_regions, char* cible) {
    for (int i = 0; i < nb_regions; i++) {
        if (strcmp(regions[i], cible) == 0)
            return i;
    }
    return -1;
}


/*renvoie la liste des couples (a,b) qui sont a coté (en utlisant 
leur numéro plutot que leur nom), avec a<b . m est nb de cpl*/
int** lire_adjacences(char* filename, int* m){
   int nb_regions =0;
   char** regions = lire_regions(filename, &nb_regions);

   int** liste = malloc(1000*sizeof(int*));
   FILE* f = fopen(filename, "r");
   *m=0;
   int reg; 
   char tamp[100];
   int ind=0;
   int region_courante = -1; //indice de region a gauche du :


   while ((reg =fgetc(f))!= EOF) {
        if (reg== ':'){ //on a lu le nom de og region
            tamp[ind]= '\0';
            ind=0;
            //indice de la region courante avec la fonction d'avant
            region_courante =chercher_region(regions, nb_regions, tamp);

        }
        else if (reg== ',' || reg=='\n'){
            if (ind>0){
                tamp[ind]= '\0';
                int voisin = chercher_region(regions, nb_regions, tamp);
                ind=0;

                //on n'ajoute la pair que si a<b pour éviter les douxblonds

                if (region_courante != -1 && voisin != -1
                    && region_courante< voisin) {
                        liste[*m] = malloc(2*sizeof(int));
                        liste[*m][0]= region_courante;
                        liste[*m][1]= voisin;
                        (*m)++;
                    }
            }
        }
        if (reg=='\n') { region_courante= -1 ;}
        
    else {
        tamp[ind]=reg;
        ind++;
    }
}
fclose(f);
return liste ;
}


/*génère "R_'region'_'couleur'" avec la couleur un chiffre"*/
char* variable_couleur(char* region, int couleur){
    char* var = malloc(50);
    sprintf(var,"R_%s_%d", region, couleur );
    return var;

}

/*au plus une (j'ai pris les fonctions d'avant de guaspar) */ 
char* au_plus_une(char** l, int n) {
    assert (n != 0);
    int taille = 0;
    int compte_couple = 0;
    for (int i = 0; i < n; i++) {
        for (int j = i+1; j<n; j++) {
            taille += strlen(l[i]) + strlen(l[j]) + 5;
            compte_couple ++;
        }
    }

    char* res = malloc((taille+ compte_couple +2) * sizeof(char));

    int k = 0; 
    res[k++] = '(';
    for (int i = 0; i < n; i++) {
        for (int j = i+1; j<n; j++) {
            res[k++] = '(';
            res[k++] = '~';
            
            int len_i = strlen(l[i]);
            for(int c = 0; c < len_i; c++) res[k++] = l[i][c];
            
            res[k++] = '|';
            res[k++] = '~';
            
            int len_j = strlen(l[j]);
            for(int c = 0; c < len_j; c++) res[k++] = l[j][c];
            
            res[k++] = ')';
            
            // Séparateur entre les paires, sauf pour la toute dernière
            if (!(i == n - 2 && j == n - 1)) {
                res[k++] = '&';
            }
        }
    }
    res[k++] = ')';
    res[k] = '\0'; 
    return res;
}

/*au moins une*/
char* au_moins_une(char** l, int n) {
    assert (n != 0);
    int taille = 0;
    for (int i = 0; i < n; i++) {
        taille += strlen(l[i]);
    }

    char* res = malloc((taille + 2 + n) * sizeof(char));

    int k = 0; 
    res[k++] = '(';
    for (int i = 0; i < n; i++) {
        int longueur = strlen(l[i]);
        for (int j = 0; j < longueur; j++) {
            res[k++] = l[i][j];
        }
        if (i < n-1) {
            res[k++] = '|' ;
        }
    }
    res[k++] = ')';
    res[k] = '\0'; 
    return res;
}


/*Pour la région 'region' ça génère d'abord
lz tableau avec ttes les couleurs puis la clause qui en select au 
moins une 
*/
char* contrainte_au_moins_une_couleur(char* region, int nb_couleurs){
    //on construit le tabl avec les variables 
    char** vars = malloc(nb_couleurs * sizeof(char*));
    for (int c = 1 ; c<=nb_couleurs; c++){
        vars[c-1] = variable_couleur(region, c);
    }
    char* clause = au_moins_une (vars, nb_couleurs);
    for (int c = 0; c<nb_couleurs; c++){
        free(vars[c]); //LIBERATION
    }
    free(vars);
    return clause;
}

/*Pour la région 'region' ça génère d'abord
lz tableau avec ttes les couleurs puis la clause qui en select au 
plus une */
char* contrainte_au_plus_une_couleur(char* region, int nb_couleurs){
    //on construit le tabl avec les variables 
    char** vars = malloc(nb_couleurs * sizeof(char*));
    for (int c = 1 ; c<=nb_couleurs; c++){
        vars[c-1] = variable_couleur(region, c);
    }
    char* clause = au_plus_une (vars, nb_couleurs);
    for (int c = 0; c<nb_couleurs; c++){
        free(vars[c]); //LIBERATION
    }
    free(vars);
    return clause;
}




char* contrainte_adjacence(char* r1, char* r2, int nb_couleurs) {
    // Taille estimée par clause : ~(R_[r1]_[i] & R_[r2]_[i]) & ...
    int taille = nb_couleurs * (strlen(r1) + strlen(r2) + 40) + 1;
    char* buffer = malloc(taille);
    char* tmp = malloc(taille);
    buffer[0] = '\0';

    for (int i = 0; i < nb_couleurs; i++) {
        sprintf(tmp, "~(R_%s_%d & R_%s_%d)", r1, i, r2, i);
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

    char* contrainte = malloc(taille + 1);
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

    char* contrainte = malloc(taille+1);
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


void afficher_coloriage(char* fichier_valuation, char** regions, int n) {
    FILE* f = fopen(fichier_valuation, "r");
    if (!f) {
        perror("afficher_coloriage: fopen");
        return;
    }

    // couleur trouvée pour chaque région
    int* couleur_region = malloc(n * sizeof(int));
    for (int i = 0; i < n; i++) couleur_region[i] = -1;

    char var[200];

    while (fscanf(f, "%s", var) == 1) {
        // var = R_[Nom]_[k]
        char region[150];
        int k;

        if (sscanf(var, "R_%[^_]_%d]", region, &k) == 2) {
            // retrouver l’indice de la région
            for (int i = 0; i < n; i++) {
                if (strcmp(regions[i], region) == 0) {
                    couleur_region[i] = k;
                }
            }
        }
    }

    fclose(f);

    printf("\n=== Coloriage obtenu ===\n");
    for (int i = 0; i < n; i++) {
        if (couleur_region[i] == -1)
            printf("%s → aucune couleur trouvée (erreur)\n", regions[i]);
        else
            printf("%s → couleur %d\n", regions[i], couleur_region[i]);
    }

    free(couleur_region);
}