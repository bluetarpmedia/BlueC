#ifdef SUPPRESS_WARNINGS
#ifdef __clang__
#pragma GCC diagnostic ignored "-Wimplicit-const-int-float-conversion"
#pragma GCC diagnostic ignored "-Wliteral-conversion"
#endif
#endif
/* Test that we correctly perform conversion as if by assignment */

int check_args(long l, float f) {
    return l == 2 && f == -6.0f;
}

float return_float(void) {
    /* Implicitly convert this integer to the nearest float,
     * which is 18446744073709551616.0
     */
    return 18446744073709551586ul;
}

int check_assignment(float arg) {
    // arg = 4.9
    int i = 0;
    /* truncate arg to 4 */
    i = arg;
    return i == 4;
}
int main(void) {

    /* function arguments: 2.4 should be truncated to 2, -6 should be converted to -6.0f */
    if (!check_args(2.4, -6)) {
        return 1;
    }

    /* return values */
    if (return_float() != 18446744073709551616.0f) {
        return 2;
    }

    /* assignment statement */
    if (!check_assignment(4.9f)) {
        return 3;
    }

    /* initializer */
    float f = 18446744073709551586ul; // implicitly convert constant to nearest float

    if (f != 18446744073709551616.f) {
        return 4;
    }

    return 0;
}