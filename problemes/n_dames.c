#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>
#include <string.h>
#include "utils.h"


//renvoie la chaine de caractères X_[i]_[j]
char* variable(int i, int j) {
    char* buffer = malloc(32);
    sprintf(buffer, "X_[%d]_[%d]", i, j);
    return buffer;
}

//formule de contrainte sur la ligne
char* contrainte_une_ligne(int i, int n){
    char** tabvar = malloc(n* sizeof(char*));
    for(int j=0 ; j<n ;j++){
        tabvar[j]=variable (i, j);
    }
    char* c1 =au_moins_une(tabvar,n);
    char* c2 = au_plus_une(tabvar, n); //on exprime les deux cond

    char* res= malloc(1024);
    sprintf(res, "(%s & %s)", c1, c2); //on associe les deux cond

    return res;
}

/*grande formule qui veut une reine sur chaque ligne*/
char* contrainte_toutes_lignes (int n){
    char* res = malloc(10000); //il en faut bcp
    strcpy(res, "("); //debut on met parenthèse

    for (int i=0; i<n; i++){ //on concatène au fur et a mesure
        char* c= contrainte_une_ligne (i,n); //les formules individuelles
        strcat(res,c);
        if (i<n-1) strcat (res, " & ");
    }
    strcat(res, ")"); //on ferme 
    return res;
}

/*au plus une reine par colonne*/
char* contrainte_une_colonne (int j, int n){
    char** tabvar = malloc(n* sizeof(char*));
    for(int i=0 ; i<n ;i++){
        tabvar[i]=variable (i, j);
    }

    char* c2 = au_plus_une(tabvar, n); 

    return c2;
}

/*grande formule qui veut au plus une reine sur chaque colonne*/
char* contrainte_toutes_colonnes (int n){
    char* res = malloc(10000); //il en faut bcp
    strcpy(res, "("); //debut on met parenthèse

    for (int i=0; i<n; i++){ //on concatène au fur et a mesure
        char* c= contrainte_une_colonne (i,n); //les formules individuelles
        strcat(res,c);
        if (i<n-1) strcat (res, " & ");
    }
    strcat(res, ")"); //on ferme 
    return res;
}

/*au plus une reine par diagonale descendante, i-j =d*/
char* contrainte_une_diagonale_desc(int d, int n) {
    char* vars[100];
    int k = 0;

    for (int i = 0; i < n; i++) {
        int j = i + d;
        if (j >= 0 && j < n) {
            vars[k++] = variable(i, j);
        }
    }

    if (k <= 1) return strdup("T"); /* aucune contrainte */
    return au_plus_une(vars, k);
}

/* une diagonale montante i +j = d) */
char* contrainte_une_diagonale_mont(int d, int n) {
    char* vars[100];
    int k = 0;

    for (int i = 0; i < n; i++) {
        int j = d - i;
        if (j >= 0 && j < n) {
            vars[k++] = variable(i, j);
        }
    }

    if (k <= 1) return strdup("T");
    return au_plus_une(vars, k);
}

/* toutes les diagonales */
char* contrainte_toutes_diagonales(int n) {
    char* res = malloc(20000);
    strcpy(res, "(");

    int first = 1;

    /* diagonales \ */
    for (int d = -n; d <= n; d++) {
        char* c = contrainte_une_diagonale_desc(d, n);
        if (!first) strcat(res, " & ");
        strcat(res, c);
        first = 0;
    }

    /* diagonales / */
    for (int d = 0; d <= 2*n; d++) {
        char* c = contrainte_une_diagonale_mont(d, n);
        strcat(res, " & ");
        strcat(res, c);
    }

    strcat(res, ")");
    return res;
}

void gen_formule_n_dames(int n, char* filename) {
    FILE* f = fopen(filename, "w");

    char* lignes = contrainte_toutes_lignes(n);
    char* colonnes = contrainte_toutes_colonnes(n);
    char* diagonales = contrainte_toutes_diagonales(n);

    char* formule = malloc(50000);

    sprintf(formule, "(%s & %s & %s)", lignes, colonnes, diagonales);

    fprintf(f, "%s", formule);

    fclose(f);
}

int main(){
  return 0;
}