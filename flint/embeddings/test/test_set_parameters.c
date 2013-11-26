#include <assert.h>
#include <stdio.h>
#include <stdlib.h>
#include <flint/nmod_poly.h>

#include "util.h"
#include "sage_output.h"
#include "nmod_poly_extra.h"
#include "embeddings.h"


/*------------------------------------------------------------*/
/* if opt = 1, runs a check                                   */
/* else, runs timings                                         */
/*------------------------------------------------------------*/
void check(int opt){
  mp_limb_t n = 65537;
  long i;
  flint_rand_t state;
  flint_randinit(state);
  
  for (i = 1; i < 100; i++){
    embeddings_t F;
    embeddings_init(F, n);
    
    nmod_poly_t P;
    nmod_poly_init(P, n);
    do
      nmod_poly_rand_dense_monic(P, state, i);
    while (! nmod_poly_is_irreducible(P));
    
    if (opt == 1){
      embeddings_set_parameters(F, P);

      sage_output_init(P->mod);
      sage_output_assign_poly(P, "P");
      sage_output_assign_poly(F->P, "FP");
      sage_output_assign_poly(F->iP, "iP");
      sage_output_assign_poly(F->SP, "SP");
      sage_output_assign_vec(F->TP->coeffs, i, "TP");

      printf("Q.<xx>=U.quo(P)\n");
      printf("all([FP==P, iP==inverse_mod(P.derivative(), P), SP==inverse_mod(P.reverse(), x^(P.degree()-1)), TP==[(xx^j).trace() for j in range(len(TP))]])\n");
    }
    else{
      double t, u;
      long j;

      t = util_gettime();
      for (j = 0; j < 1000; j++)
	embeddings_set_parameters(F, P);
      t = util_gettime() - t;


      nmod_poly_t tmp1, tmp2, tmp3;
      nmod_poly_init(tmp1, n);
      nmod_poly_init(tmp2, n);
      nmod_poly_init(tmp3, n);

      nmod_poly_rand_dense(tmp1, state, i);
      nmod_poly_rand_dense(tmp2, state, i);

      u = util_gettime();
      for (j = 0; j < 1000; j++)
	nmod_poly_mul(tmp3, tmp1, tmp2);
      u = util_gettime() - u;

      nmod_poly_clear(tmp1);
      nmod_poly_clear(tmp2);
      nmod_poly_clear(tmp3);

      printf("%lu %f %f\n", i, t, u);

    }

    embeddings_clear(F);
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


