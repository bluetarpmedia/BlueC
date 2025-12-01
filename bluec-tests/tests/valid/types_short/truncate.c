int main(void) {
    /* Test 1: int to short truncation (70000 -> 70000 - 65536 = 4464) */
    int v1 = 70000;
    short s1 = v1;
    if (s1 != 4464) return 1;

    /* Test 2: int to short truncation producing negative (40000 -> 40000 - 65536 = -25536) */
    int v2 = 40000;
    short s2 = v2;
    if (s2 != -25536) return 2;

    /* Test 3: long to short truncation (131071 -> 131071 - 2*65536 = -1) */
    long v3 = 131071L;
    short s3 = v3;
    if (s3 != -1) return 3;

    /* Test 4: long to short exact wrap to zero (65536 -> 0) */
    long v4 = 65536L;
    short s4 = v4;
    if (s4 != 0) return 4;

    /* Test 5: negative int to short truncation (-70000 -> -70000 + 65536 = -4464) */
    int v5 = -70000;
    short s5 = v5;
    if (s5 != -4464) return 5;

    /* Test 6: large shifted value truncated (1 << 20 -> 1048576 -> 0 modulo 65536) */
    int v6 = 1 << 20;
    short s6 = v6;
    if (s6 != 0) return 6;

    /* Test 7: assign result of promoted arithmetic back to short (30000 + 30000 = 60000 -> -5536) */
    short a7 = 30000;
    short b7 = 30000;
    int sum7 = a7 + b7;
    short s7 = sum7;
    if (s7 != -5536) return 7;

    /* Test 8: value within short range is preserved (12345) */
    int v8 = 12345;
    short s8 = v8;
    if (s8 != 12345) return 8;

    return 0;
}
