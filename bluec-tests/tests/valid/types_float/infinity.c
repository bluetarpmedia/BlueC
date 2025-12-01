/* Test our handling of positive and negative infinity */

#ifdef SUPPRESS_WARNINGS
#ifdef __clang__
#pragma clang diagnostic ignored "-Wliteral-range"
#else
#pragma GCC diagnostic ignored "-Woverflow"
#endif
#endif

/* This value should be rounded to infinity */
float inf = 3.5e38f;
/* This should round to the largest finite float */
float very_large = 3.40282347E+38f;
float zero = 0.0f;
int main(void) {

    /* Rounding constants to infinity */

    // 11e330 should be rounded to infinity
    if (inf != 11e330f) {
        return 1;
    }

    /* Infinity compares greater than any finite number */
    if (inf <= very_large) {
        return 2;
    }

    /* Calculations that result in infinity */

    /* Multiplication result is too large to represent as finite value */
    if(very_large * 10.0f != inf) {
        return 3;
    }

    /* 1/0 is infinity */
    if (1.0f / zero != inf) {
        return 4;
    }

    /* Negative infinity */
    float negated_inf = -inf;
    float negated_inf2 = -1.0f / zero;

    /* Negative infinity compares less than any finite number */
    if (negated_inf >= -very_large) {
        return 5;
    }

    if (negated_inf != negated_inf2) {
        return 6;
    }

    return 0;
}