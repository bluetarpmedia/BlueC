int main(void) {
    // Test 1: float -> int (small value should round toward zero)
    float f1 = 42.9f;
    int i1 = (int)f1;
    if (i1 != 42)
        return 1;

    // Test 2: double -> unsigned int (small positive value)
    double d2 = 123.0;
    unsigned int u2 = (unsigned int)d2;
    if (u2 != 123u)
        return 2;

    // Test 3: float -> long long (large but representable)
    float f3 = 1.0e6f; // fits in 64-bit integer
    long long ll3 = (long long)f3;
    if (ll3 != 1000000LL)
        return 3;

    // Test 4: double -> unsigned long long (large but representable)
    double d4 = 4294967295.0; // max 32-bit unsigned
    unsigned long long ull4 = (unsigned long long)d4;
    if (ull4 != 4294967295ULL)
        return 4;

    // Test 5: int -> float (small exact value survives round-trip)
    int i5 = 123456;
    float f5 = (float)i5;
    int back5 = (int)f5;
    if (i5 != back5)
        return 5;

    // Test 6: long long -> double
    long long ll6 = 123456789012345LL;
    double d6 = (double)ll6;
    long long back6 = (long long)d6;
    if (ll6 != back6)
        return 6;

    // Test 7: unsigned long long -> float
    unsigned long long ull7 = 18446744073709551615ULL;
    float f7 = (float)ull7;
    if (f7 != 18446744073709551616.0f)
        return 7;

    // Test 8: negative int -> float (should preserve sign)
    int i8 = -42;
    float f8 = (float)i8;
    if (f8 != -42.0f)
        return 8;

    // Test 9: round trip unsigned long -> float and back
    unsigned long ul9 = 16777216ul;
    float f9 = (float)ul9;
    unsigned long back9 = (unsigned long)f9;
    if (back9 != ul9) {
        return 9;
    }

    // Test 10: round trip short -> float and back
    short s10 = -100;
    float f10 = (float)s10;
    short back10 = (short)f10;
    if (back10 != s10) {
        return 10;
    }

    // Test 11: round trip unsigned short -> double and back
    unsigned short us11 = 30000;
    double f11 = (double)us11;
    unsigned short back11 = (unsigned short)f11;
    if (back11 != us11) {
        return 10;
    }

    return 0; // success
}
