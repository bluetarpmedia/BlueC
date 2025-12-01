// Defined in dependencies/math.c
int float_isnan(float f);
int float_isinf(float f);
double double_fabs(double d);

int main(void) {
    float f = 1.234567f;
    double d = (double)f;

    // Test 1: float -> double preserves value closely
    if (double_fabs(d - 1.234567) > 1e-6)
        return 1;

    // Test 2: double too large -> float becomes +infinity
    double big = 1.0e40;
    float f2 = (float)big;
    if (!float_isinf(f2) || f2 <= 0.0f)
        return 2;

    // Test 3: negative double too large -> float becomes -infinity
    double negbig = -1.0e40;
    float f3 = (float)negbig;
    if (!float_isinf(f3) || f3 >= 0.0f)
        return 3;

    // Test 4: NaN propagation
    double nanval = 0.0 / 0.0; // produces NaN
    float f4 = (float)nanval;
    if (!float_isnan(f4))
        return 4;

    // Test 5: round‑trip cast loses precision
    double precise = 123456789.0;
    float f5 = (float)precise;
    double back = (double)f5;
    if (precise == back) // should not be equal
        return 5;

    // Test 6: small integer within float precision
    double smallint = 12345.0;
    float f6 = (float)smallint;
    double back6 = (double)f6;
    if (smallint != back6) // should be equal
        return 6;

    // Test 7: exact power of two within float range
    double pow2 = 1048576.0; // 2^20
    float f7 = (float)pow2;
    double back7 = (double)f7;
    if (pow2 != back7)
        return 7;

    // Test 8: fractional value that fits in float precision
    double frac = 0.5; // exactly representable
    float f8 = (float)frac;
    double back8 = (double)f8;
    if (frac != back8)
        return 8;

    // Test 9: small negative integer
    double negsmall = -42.0;
    float f9 = (float)negsmall;
    double back9 = (double)f9;
    if (negsmall != back9)
        return 9;

    // Test 10: zero round‑trip
    double zero = 0.0;
    float f10 = (float)zero;
    double back10 = (double)f10;
    if (zero != back10)
        return 10;

    return 0; // success
}
