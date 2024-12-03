#include <stdio.h>
#include <stdlib.h>
#include <math.h>

void CSR_Dim(double **Mat, int n, int m, int *n0, double thre) {
    *n0 = 0;
    for (int i = 0; i < n; i++) {
        for (int j = 0; j < m; j++) {
            if (fabs(Mat[i][j]) >= thre) {
                (*n0)++;
            }
        }
    }
}

void Mat_to_CSR(double **Mat, int n, int m, double *Va, int *Ja, int *Ia, double thre, int io) {
    int KK = 0;
    for (int i = 0; i <= n; i++) {
        Ia[i] = 0;
    }
    Ia[0] = 1;
    for (int i = 0; i < n; i++) {
        for (int j = 0; j < m; j++) {
            if (fabs(Mat[i][j]) >= thre) {
                Va[KK] = Mat[i][j];
                Ja[KK] = j + 1;
                KK++;
            }
        }
        Ia[i + 1] = KK + 1;
    }
}

void CSR_to_Mat(double *Va, int *Ja, int *Ia, double **Mat, int n, int m, int io) {
    for (int i = 0; i < n; i++) {
        for (int j = 0; j < m; j++) {
            Mat[i][j] = 0.0;
        }
    }
    for (int i = 0; i < n; i++) {
        int k1 = Ia[i] - 1;
        int k2 = Ia[i + 1] - 1;
        for (int j = k1; j < k2; j++) {
            Mat[i][Ja[j] - 1] = Va[j];
        }
    }
}

void amub(int nrow, int ncol, double *a, int *ja, int *ia, double *b, int *jb, int *ib, double *c, int *jc, int *ic) {
    int *iw = (int *)malloc(ncol * sizeof(int));
    int len = 0;
    ic[0] = 1;
    for (int i = 0; i < ncol; i++) {
        iw[i] = 0;
    }

    for (int ii = 0; ii < nrow; ii++) {
        for (int ka = ia[ii] - 1; ka < ia[ii + 1] - 1; ka++) {
            double scal = a[ka];
            int jj = ja[ka] - 1;
            for (int kb = ib[jj] - 1; kb < ib[jj + 1] - 1; kb++) {
                int jcol = jb[kb] - 1;
                int jpos = iw[jcol];
                if (jpos == 0) {
                    len++;
                    jc[len - 1] = jcol + 1;
                    iw[jcol] = len;
                    c[len - 1] = scal * b[kb];
                } else {
                    c[jpos - 1] += scal * b[kb];
                }
            }
        }
        for (int k = ic[ii] - 1; k < len; k++) {
            iw[jc[k] - 1] = 0;
        }
        ic[ii + 1] = len + 1;
    }

    free(iw);
}


