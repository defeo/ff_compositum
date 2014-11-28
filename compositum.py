# -*- encoding: utf-8
from sage.matrix.berlekamp_massey import berlekamp_massey
from sage.rings.laurent_series_ring import LaurentSeriesRing
from sage.modules.free_module_element import free_module_element as vector

def ffext(k, n_or_modulus, name='z'):
    '''Creates an extension of degree n of the (non-prime) finite field K.

    If the second parameter is an integer n, it must be prime with the
    degree of k over the prime field.

    If it is a polynomial, it must be irreducible, have coefficients
    in the prime field, and its degree must be prime with the degree
    of k over the prime field.

    Returns

    - The extension field K,
    - an injection φ : k → K,
    - the section of φ.
    '''
    Fp = k.prime_subfield()
    m = k.degree()
    FpX = Fp.polynomial_ring(name)
    P = k.polynomial()
    
    if n_or_modulus.parent() is FpX:
        Q = n_or_modulus
        n = Q.degree()
    else:
        n = n_or_modulus
        Q = FpX.irreducible_element(n)
    assert(n.gcd(m) == 1)
    
    R = _composed_product(P,Q)
    K = Fp.extension(R, name)

    iR = K(R.derivative())**-1
    Pt = _trace_from_minpoly(P, m)
    SP = _S(P, P.degree() - 1)
    uQ = _trace_from_minpoly(Q, m*n)
    onestar = _trem(FpX(1), Q, m*n)
    
    def phi(x):
        ell = mono_to_dual(x, k, Pt, SP)
        ell = _trem(ell, P, m*n)
        ell = [l*u for l,u in zip(ell, uQ)]
        return dual_to_mono(FpX(ell), K, iR)
    
    def phi_inv(x):
        return k(FpX([c*u for c,u in zip(x.polynomial(), onestar)]))
    
    return K, phi, phi_inv


def _S(P, m):
    '1/rev(P) mod x^m'
    PP = P.parent()
    x = PP.gen()
    LS = LaurentSeriesRing(PP.base_ring(), 'z', default_prec=m)
    irP = 1/LS(P.reverse())
    return PP(irP.list())

def _trace_from_minpoly(M, k):
    'computes the k first Newton sums of M (as a polynomial)'
    m = M.degree()
    tmp = M.derivative().reverse(m-1)*_S(M, k) ## m-1 = degree
    return tmp.truncate(k)

def _composed_product(P, Q):
    'composed product of P and Q'
    m = P.degree()
    n = Q.degree()
    t = _trace_from_minpoly(P, 2*m*n+4)
    u = _trace_from_minpoly(Q, 2*m*n+4)
    v = [t[i]*u[i] for i in range(2*m*n+4)]
    BM = berlekamp_massey(v)
    return BM

def _tmul(A, B, m, k):
    '''
    univariate transposed product
    K^{m+k} -> K^k, with deg(B) <= m
    '''
    return ((A*(B.reverse(m))).truncate(k+m)).shift(-m) ## m = degree

def _trem(ell, P, k):
    'transposed remainder. ell is reduced mod P, outputs k terms'
    m = P.degree()
    U = P.parent()
    G = _S(P, k-m)
    A = _tmul(ell, P, m, k-m)
    C = (G*A).truncate(k-m)
    return ell - C.shift(m)

def _tmulmod(ell, B, P, S):
    '''
    transposed modular multiplication, B.ell mod P
    S = _S(P, P.degree()-1), precomputed
    '''
    m = P.degree()
    U = P.parent()
    A = _tmul(U(ell), P, m, m-1)
    C = (S*A).truncate(m-1)
    D = U(ell) - C.shift(m)
    return _tmul(D, U(B), m-1, m)

def mono_to_dual(x, k, Mt, SM):
    return _tmulmod(Mt, x, k.polynomial(), SM)

def dual_to_mono(x, k, iM):
    '''
    given traces tr(A B^i) and the min poly M of B,
    recovers the expression A = C(B)
    '''
    m = k.degree()
    M = k.polynomial()
    N = (M.reverse(m) * x).truncate(m)
    Nstar = k(N.reverse(m-1))
    C = Nstar * iM
    return C
