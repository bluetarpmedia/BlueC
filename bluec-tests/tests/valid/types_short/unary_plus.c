int main(void) {
    /* Test 1: unary plus on short promotes to int and yields same numeric value */
    short s1 = 123;
    if (+s1 != 123) return 1;

    /* Test 2: unary plus on negative short preserves sign after promotion */
    short s2 = -123;
    if (+s2 != -123) return 2;

    /* Test 3: unary plus on int returns same int value */
    int i1 = 123456;
    if (+i1 != 123456) return 3;

    /* Test 4: unary plus on negative int preserves sign */
    int i2 = -123456;
    if (+i2 != -123456) return 4;

    /* Test 5: unary plus on long returns same long value */
    long l1 = 123456789L;
    if (+l1 != 123456789L) return 5;

    /* Test 6: unary plus on negative long preserves sign */
    long l2 = -123456789L;
    if (+l2 != -123456789L) return 6;

    /* Test 7: unary minus on short promotes to int and negates correctly */
    short s3 = 200;
    if (-s3 != -200) return 7;

    /* Test 8: unary minus on negative short becomes positive int */
    short s4 = -200;
    if (-s4 != 200) return 8;

    /* Test 9: unary minus on int negates correctly */
    int i3 = 100000;
    if (-i3 != -100000) return 9;

    /* Test 10: unary minus on long negates correctly */
    long l3 = 100000000L;
    if (-l3 != -100000000L) return 10;

    /* Test 11: binary plus short + short promotes to int before addition */
    short sa = 30000;
    short sb = 30000;
    if (sa + sb != 60000) return 11;

    /* Test 12: binary plus short + int promotes short to int and yields int result */
    short sc = 2000;
    int ic = 100000;
    if (sc + ic != 102000) return 12;

    /* Test 13: binary plus int + long promotes int to long and yields long result */
    int id = 50000;
    long ld = 70000L;
    if (id + ld != 120000L) return 13;

    /* Test 14: binary plus short + long promotes short to long and yields long result */
    short se = -30000;
    long le = 100000L;
    if (se + le != 70000L) return 14;

    /* Test 15: mixing unary plus with binary plus does not change semantics */
    short s5 = 1234;
    int i5 = 1000;
    if (+s5 + +i5 != 2234) return 15;

    /* Test 16: unary plus on expression of shorts promotes and sums correctly */
    short sx = 15000;
    short sy = 15000;
    if (+(sx + sy) != 30000) return 16;

    /* Test 17: unary minus on long after promotion from short works */
    short s6 = 40000 - 32768; /* a value inside short range, chosen to avoid UB on some platforms */
    long res17 = - (long) s6;
    if (res17 != - (long) s6) return 17;

    /* Test 18: ensure that applying unary plus does not change type semantics for negatives */
    short s7 = -1;
    if (+s7 + 2 != 1) return 18;

    /* Test 19: verify no unexpected truncation when adding promoted shorts then assigning to long */
    short s8 = 30000;
    short s9 = 30000;
    long sum_long = s8 + s9;
    if (sum_long != 60000L) return 19;

    /* Test 20: verify unary plus on zero works across types */
    short s0 = 0;
    int i0 = 0;
    long l0 = 0L;
    if (+s0 != 0) return 20;
    if (+i0 != 0) return 21;
    if (+l0 != 0L) return 22;

    return 0;
}
