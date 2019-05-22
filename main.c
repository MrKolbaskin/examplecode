#include <stdio.h>
#include <stdlib.h>

int n;

// Функция, которая нормализует строки матрицы
void normalize (double **array, int beg)
{
    for (int i = beg; i < n; i++) {
        double tmp = array[i][beg];
        if (tmp == 0) {
            continue;
        }
        for (int j = beg; j < n + 1; j++) {
            array[i][j] /= tmp;
        }
    }
}

//Функция которая складывает строки с множителем (-1)
void sum (double **array, int beg)
{
    for (int i = beg + 1; i < n; i++) {
        if (array[i][beg] == 0) {
            continue;
        }
        for (int j = beg; j < n + 1; j++) {
            array[i][j] -= array[beg][j];
        }
    }
}

//Функция нахождения решения СЛАУ методом Гаусса
void
simple_Gaus (double **coef)
{
    for (int i = 0; i < n; i++){
        normalize(coef, i);
        sum(coef, i);
    }
    double x[n];
    for (int i = n - 1; i >= 0; i--) {
        x[i] = coef[i][n];
        for (int j = i + 1; j < n; j++) {
            x[i] -= coef[i][j] * x[j];
        }
    }
    for (int i = 0; i < n; i++) {
        printf("x%d = %lf ", i + 1, x[i]);
    }
}

void
change (double **coef, int num, int str)
{
    if (num == str) {
        return;
    } else {
        double tmp;
        for (int i = 0; i < n; i++) {
            tmp = coef[str][i];
            coef[str][i] = coef[num][i];
            coef[num][i] = tmp;
        }
        tmp = coef[str][n];
        coef[str][n] = coef[num][n];
        coef[num][n] = tmp;
    }
}

//Функция нахождения решения СЛАУ методом Гаусса методом выбора главного элемента
void
main_Gauss (double **coef)
{
    for (int j = 0; j < n; j++){
        double max = coef[j][j];
        int num = j;
        for (int i = j + 1; i < n; i++) {
            if (coef[i][j] > max) {
                num = j;
                max = coef[i][j];
            }
        }
        change(coef, num, j);
        normalize(coef, j);
        sum(coef, j);
    }
    double x[n];
    for (int i = n - 1; i >= 0; i--) {
        x[i] = coef[i][n];
        for (int j = i + 1; j < n; j++) {
            x[i] -= coef[i][j] * x[j];
        }
    }
    for (int i = 0; i < n; i++) {
        printf("x%d = %lf ", i + 1, x[i]);
    }
}


void
determinant(double **coef)// Расчет определителя
{
    double det = 1;
    for (int i = 0; i < n - 1; i++) {
        double tmp = coef[i][i];

        for (int j = i + 1; j < n; j++) {
            double sup = coef[j][i] / tmp;
            for (int t = i; t < n; t++) {
                coef[j][t] -= sup * tmp;
            }
        }
    }
    for (int i = 0; i < n; i++) {
        det *= coef[i][i];
    }
    printf("%lf\n", det);
}

int main(void) {
    scanf("%d", &n);
    double **coef = malloc (sizeof(double*) * n);
    for (int i = 0; i < n; i++) {
        coef[i] = malloc (sizeof(double) * (n + 1));
    }
    for (int i = 0; i < n; i++) {
        for (int j = 0; j < n + 1; j++) {
            scanf("%lf", &coef[i][j]);
        }
    }
    determinant(coef);
    printf("\n");
    for (int i = 0; i < n; i++) {
        for (int j = 0; j < n + 1; j++) {
            printf("%lf ", coef[i][j]);
        }
        printf("\n");
    }
    return 0;
}