#include <assert.h>
#include <stdio.h>
#include <stdlib.h>
#include <flint/nmod_poly.h>

#include "util.h"
#include "sage_output.h"
#include "nmod_poly_extra.h"

/*------------------------------------------------------------*/
/* if opt = 1, runs a check                                   */
/* else, runs timings                                         */
/*------------------------------------------------------------*/
void check(int opt){
  long i;
  flint_rand_t state;
  flint_randinit(state);
  mp_limb_t n = 65537;

  for (i = 2; i < 400; i+=13){
    long len = 2*i+3;
    nmod_poly_t P;
    mp_ptr newtonP, roots;

    nmod_poly_init(P, n);
    roots = _nmod_vec_init(i);
    _nmod_vec_randtest(roots, state, i, P->mod);
    nmod_poly_product_roots_nmod_vec(P, roots, i);
	
    newtonP = _nmod_vec_init(len);
    
    if (opt == 1){
      nmod_poly_to_newton_sums(newtonP, P, len);
      sage_output_init(P->mod);
      sage_output_assign_vec(newtonP, len, "newtonP");
      sage_output_assign_vec(roots, i, "roots");
      printf("newtonP == [add([r^i for r in roots]) for i in range(len(newtonP))]\n");
    }
    else{
      double t, u;
      long j;
      t = util_gettime();
      for (j = 0; j < 10000; j++)
	nmod_poly_to_newton_sums(newtonP, P, i);
      t = util_gettime() - t;

      nmod_poly_t tmp1, tmp2, tmp3;
      nmod_poly_init(tmp1, n);
      nmod_poly_init(tmp2, n);
      nmod_poly_init(tmp3, n);

      nmod_poly_rand_dense(tmp1, state, i);
      nmod_poly_rand_dense(tmp2, state, i);

      u = util_gettime();
      for (j = 0; j < 10000; j++)
	nmod_poly_mul(tmp3, tmp1, tmp2);
      u = util_gettime() - u;

      nmod_poly_clear(tmp1);
      nmod_poly_clear(tmp2);
      nmod_poly_clear(tmp3);

      printf("%lu %f %f\n", i, t, u);
    }

    _nmod_vec_clear(newtonP);
    _nmod_vec_clear(roots);
    nmod_poly_clear(P);
  }

  flint_randclear(state);
}

/*------------------------------------------------------------*/
/* main just calls check()                                    */
/* if not argument is given, runs timings                     */
/* if the argument 1 is given, runs check                     */
/*------------------------------------------------------------*/
int main(int argc, char **argv){
  int opt = 0;
  if (argc > 1)
    opt = atoi(argv[1]);
  check(opt);
  return 0;
}

