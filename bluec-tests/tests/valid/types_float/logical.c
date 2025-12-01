/* Test floats in &&, ||, ! and controlling expressions */

#ifdef SUPPRESS_WARNINGS
#ifdef __clang__
#pragma clang diagnostic ignored "-Wliteral-conversion"
#pragma clang diagnostic ignored "-Wliteral-range"
#else
#pragma GCC diagnostic ignored "-Woverflow"
#endif
#endif

float zero = 0.0f;
float non_zero = 1E-20f;
float one = 1.0f;
// this number is so small it will be rounded to zero
float rounded_to_zero = 1e-330f;

int main(void) {

    /* double as controlling expression in if statement */

    if (zero) {
        return 1;
    }

    if (rounded_to_zero) {
        return 2;
    }

    if (non_zero) {
        // no-op; should take this one
    } else {
        return 3;
    }

    /* constant doubles as controlling expression */
    if (0.e10f) {
        return 4;
    }

    /* ! operator */
    if (!non_zero) {
        return 5;
    }

    if (!(!zero)) {
        return 6;
    }

    if (!(!rounded_to_zero)) {
        return 7;
    }

    /* && operator - test w/ mix of floating-point and non-floating-point operands */

    if (!(non_zero && 1.0f)) {
        return 8;
    }

    if (3.0f && zero) {
        return 9;
    }

    if (rounded_to_zero && 1000e10f) {
        return 10;
    }


    // mix of double and integer operands
    if (18446744073709551615UL && zero) {
        return 11;
    }

    if (!(non_zero && 5l)) {
        return 12;
    }


    /* || operator */

    if (!(5.0f || zero)) {
        return 13;
    }

    if (zero || rounded_to_zero) {
        return 14;
    }

    if (!(rounded_to_zero || 0.0001f)) {
        return 15;
    }

    // mix of double and integer operands
    if (!(non_zero || 0u)) {
        return 16;
    }

    // try || with two constants
    if (!(0 || 0.0000005f)) {
        return 17;
    }

    return 0;

}
