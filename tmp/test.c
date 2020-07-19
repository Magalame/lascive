#include <string.h>

#include <stdio.h>
#include <stdlib.h>

#define regsA 3   
#define regsB 4

typedef double double4 __attribute__((ext_vector_type(4)));

#define LoadDouble4(PTR) *((const double4 *)(PTR))
#define BroadcastDouble4(VAL) ((double4)(VAL))

inline void StoreuDouble4(double *p, double4 v) { memcpy(p, &v, sizeof(double4)); }
/// Perform an unaligned store to a double pointer.


inline double4 LoaduDouble4(const double *p) {
  double4 res;
  memcpy(&res, p, sizeof(double4));
  return res;
}

inline void AdduDouble4(double *p, double4 v) {
  StoreuDouble4(p, LoaduDouble4(p) + v);
}


typedef struct Matrix {
   int nrows;
   int ncols;
   double* arr;
} Matrix;

inline double* elem(Matrix* m, int i, int j){
    return m->arr + i*m->ncols + j;
}

void kernel(Matrix* a, Matrix* b, Matrix* c) {

  double4 csum[regsA][regsB] = {{0.0}};

  for (int bi=0; bi < regsB; bi++){
    double4 bb = LoadDouble4(elem(b, 0, bi*4));

    for (int ai=0; ai < regsA; ai++) {

      double4 aa = BroadcastDouble4(*elem(a,ai,0));

      csum[ai][bi] += aa * bb;

    }

  }

  for (int ai = 0; ai < regsA; ai++) {
    for (int bi = 0; bi < regsB; bi++) {
      AdduDouble4(elem(c, ai, bi * 4), csum[ai][bi]);
    }
  }

}

int main () {
  double* arra = calloc(3, sizeof(double));
  for (int i=0; i<3;i++){
    *(arra + i) = 1;
  }
  double* arrb = calloc(4*4, sizeof(double));
  for (int i=0; i<4*4;i++){
    *(arrb + i) = 1;
  }
  double* arrc = calloc(3*4*4, sizeof(double));
  for (int i=0; i<3*4*4;i++){
    *(arrc + i) = 0;
  }

  Matrix ma = {3, 1, arra};
  Matrix mb = {1, 16, arrb};
  Matrix mc = {3,16,arrc};

  kernel(&ma, &mb, &mc);

  return 0;
}