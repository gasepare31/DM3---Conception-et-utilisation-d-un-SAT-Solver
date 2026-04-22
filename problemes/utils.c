#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <assert.h>

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

void test () {
    char* test1[] = {"a"};
    char* res1 = au_moins_une(test1, 1);
    assert(strcmp(res1, "(a)") == 0);
    free(res1);

    char* arr4[] = {"foo", "bar", "baz", "qux"};
    char* res4 = au_moins_une(arr4, 4);
    assert(strcmp(res4, "(foo|bar|baz|qux)") == 0);
    free(res4);

    char* arr5[] = {"a", "b"};
    char* res5 = au_plus_une(arr5, 2);
    assert(strcmp(res1, "((~a|~b))") == 0);
    free(res1);
}