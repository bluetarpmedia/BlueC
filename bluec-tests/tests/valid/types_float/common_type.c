/* Test that we correctly find the common type in expressions involving floats */

#ifdef SUPPRESS_WARNINGS
#ifdef __clang__
#pragma clang diagnostic ignored "-Wimplicit-const-int-float-conversion"
#else
#pragma GCC diagnostic ignored "-Wsign-compare"
#endif
#endif

int lt(float f, long l) {
    // l is implicitly converted to a float
    return f < l;
}

float tern_float_flag(float flag) {
    /* Ternary expression where controlling condition is a float
     * You do not have to convert second and third operands to float;
     * instead, we convert them to their common type, which is unsigned long,
     * THEN convert that to a float.
     * Converting -30 to unsigned long gives us 2^64 - 30, or 18446744073709551586.
     * The nearest float to this result is 18446744073709551616.0
     */
    return (float) (flag ? -30 : 10ul);
}

float tern_float_result(int flag) {
    /* The common type of the two operands is float,
     * so if flag is 0, this will return the nearest representable
     * float to 9223372036854777850ul, which is 9223372036854775808.0f
     */
    return flag ? 5.0f : 9223372036854777850ul;
}
int ten = 10;
int multiply(void) {
    /* This should promote 10 to a float,
     * calculate 10.75f * 10.0f, which is 107.5f,
     * and then truncate back to an int, 107.
     * It should not truncate 10.75 to 10 before
     * performing the calculation.
     */
    int i = 10.75f * ten;

    return i == 107;
}

int main(void) {

    /* Comparison:
     * we'll implicitly convert the long argument the nearest float,
     * which is -9007199254751228.0, so these values compare equal
     */
    if (lt(-9007199254751228.0, -9007199254751227l)) {
        return 1;
    }

    /* Ternary expressions */
    if (tern_float_flag(20.0) != 18446744073709551586.0f) {
        return 2;
    }

    if (tern_float_flag(0.0) != 10.0f) {
        return 3;
    }

    if (tern_float_result(1) != 5.0f) {
        return 4;
    }
    if (tern_float_result(0) != 9223372036854775808.0f) {
        return 5;
    }

    /* Multiplication */
    if (!multiply()) {
        return 6;
    }
    return 0;
}