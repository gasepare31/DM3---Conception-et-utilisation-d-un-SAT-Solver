#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>
#include <string.h>
#include <assert.h>

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
    fclose(f);
    return liste ;
}
}


/*génère "R_'region'_'couleur'" avec la couleur un chiffre"*/
char* variable_couleur(char* region, int couleur){
    char* var = malloc(50);
    sprintf(var,"R_[%s]_[%d]", region, couleur );
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


int main(){
    return 0;
}