int main(void) {
    /* Test 1: += with small positive value */
    short s1 = 10;
    s1 += 5; /* 15 */
    if (s1 != 15) return 1;

    /* Test 2: -= with small positive value */
    short s2 = 20;
    s2 -= 7; /* 13 */
    if (s2 != 13) return 2;

    /* Test 3: *= with values that fit in int before conversion back to short */
    short s3 = 100;
    s3 *= 3; /* 300 */
    if (s3 != 300) return 3;

    /* Test 4: /= integer division */
    short s4 = 25;
    s4 /= 5; /* 5 */
    if (s4 != 5) return 4;

    /* Test 5: %= modulus */
    short s5 = 29;
    s5 %= 6; /* 5 */
    if (s5 != 5) return 5;

    /* Test 6: <<= left shift (use small shift amount) */
    short s6 = 1;
    s6 <<= 3; /* 8 */
    if (s6 != 8) return 6;

    /* Test 7: >>= right shift (use positive so behavior is defined across implementations) */
    short s7 = 32;
    s7 >>= 2; /* 8 */
    if (s7 != 8) return 7;

    /* Test 8: &= bitwise and using decimal literals */
    short s8 = 240; /* was 0x0F0 */
    s8 &= 15;       /* 240 & 15 = 0 */
    if (s8 != 0) return 8;

    /* Test 9: |= bitwise or using decimal literals */
    short s9 = 16;  /* was 0x010 */
    s9 |= 4;        /* 16 | 4 = 20 */
    if (s9 != 20) return 9;

    /* Test 10: ^= bitwise xor using decimal literals */
    short s10 = 10; /* was 0x0A */
    s10 ^= 3;       /* 10 ^ 3 = 9 */
    if (s10 != 9) return 10;

    /* Test 11: compound assignment with a larger right-hand side that requires conversion back to short
       (30000 += 40000 -> promoted sum 70000 then stored into short) */
    short s11 = 30000;
    s11 += 40000; /* result stored back into short */
    if (s11 != (short)(30000 + 40000)) return 11;

    /* Test 12: compound assignment with negative values (signed behavior preserved through promotion) */
    short s12 = -100;
    s12 -= 50; /* -150 */
    if (s12 != -150) return 12;

    /* Test 13: sequence of compound assignments combined (ensure left-to-right evaluation and persistence) */
    short s13 = 2;
    s13 *= 3;  /* 6 */
    s13 += 4;  /* 10 */
    s13 <<= 1; /* 20 */
    if (s13 != 20) return 13;

    return 0; /* success */
}
