int main(void) {
    /* Test 1: Float literal with 'f' suffix */
    float f1 = 3.14f;
    if (f1 != 3.14f) {
        return 1;
    }

    /* Test 2: Double literal (no suffix) */
    double d1 = 2.718281828;
    if (d1 != 2.718281828) {
        return 2;
    }

    /* Test 3: Long double literal with 'L' suffix */
    long double ld1 = 1.414213562373095L;
    if (ld1 != 1.414213562373095L) {
        return 3;
    }

    /* Test 4: Hexadecimal float literal equals decimal */
    float f2 = 0x1.0p0f;  /* 1.0 * 2^0 = 1.0 */
    if (f2 != 1.0f) {
        return 4;
    }

    /* Test 5: Hex float with fractional part */
    float f3 = 0x1.8p0f;  /* 1.5 * 2^0 = 1.5 */
    if (f3 != 1.5f) {
        return 5;
    }

    /* Test 6: Hex float with positive exponent */
    float f4 = 0x1.0p3f;  /* 1.0 * 2^3 = 8.0 */
    if (f4 != 8.0f) {
        return 6;
    }

    /* Test 7: Hex float with negative exponent */
    float f5 = 0x1.0p-2f;  /* 1.0 * 2^-2 = 0.25 */
    if (f5 != 0.25f) {
        return 7;
    }

    /* Test 8: Hex double literal */
    double d2 = 0x1.921fb54442d18p1;  /* pi (approximately) */
    if (d2 != 3.141592653589793) {
        return 8;
    }

    /* Test 9: Hex long double literal */
    long double ld2 = 0x1.0p10L;  /* 1.0 * 2^10 = 1024.0 */
    if (ld2 != 1024.0L) {
        return 9;
    }

    /* Test 10: Complex hex float */
    float f6 = 0x1.5p4f;  /* 1.3125 * 2^4 = 21.0 */
    if (f6 != 21.0f) {
        return 10;
    }

    /* Test 11: Hex float with uppercase P */
    float f7 = 0x1.0P-3f;  /* 1.0 * 2^-3 = 0.125 */
    if (f7 != 0.125f) {
        return 11;
    }

    /* Test 12: Scientific notation float */
    float f8 = 1.23e2f;  /* 1.23 * 10^2 = 123.0 */
    if (f8 != 123.0f) {
        return 12;
    }

    /* Test 13: Scientific notation with negative exponent */
    double d3 = 5.0e-3;  /* 5.0 * 10^-3 = 0.005 */
    if (d3 != 0.005) {
        return 13;
    }

    /* Test 14: Hex float representing exact value */
    float f9 = 0x1.99999ap-4f;  /* Exact representation of 0.1 in float */
    float f10 = 0.1f;
    if (f9 != f10) {
        return 14;
    }

    /* Test 15: Large hex double */
    double d4 = 0x1.0p20;  /* 1.0 * 2^20 = 1048576.0 */
    if (d4 != 1048576.0) {
        return 15;
    }

    /* Test 16: Hex with multiple hex digits in mantissa */
    float f11 = 0x1.fffffep127f;  /* Close to max float */
    if (f11 <= 3.4e38f || f11 >= 3.5e38f) {
        return 16;
    }

    /* Test 17: Zero in hex form */
    double d5 = 0x0.0p0;
    if (d5 != 0.0) {
        return 17;
    }

    /* Test 18: Negative hex float */
    float f12 = -0x1.0p1f;  /* -2.0 */
    if (f12 != -2.0f) {
        return 18;
    }

    /* Test 19: Long double with 'l' (lowercase) suffix */
    long double ld3 = 2.5l;
    if (ld3 != 2.5L) {
        return 19;
    }

    /* Test 20: Verify float and double are different types */
    /* Float has less precision, so this should work */
    float f13 = 0.1f;
    double d6 = 0.1;
    /* Compare as doubles - they should be different due to precision */
    if ((double)f13 == d6) {
        return 20;
    }

    return 0;
}
