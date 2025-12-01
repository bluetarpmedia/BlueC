#define BC_SHRT_MAX (32767)

int main(void) {
    /* Test 1: postfix increment returns original value, variable incremented afterwards */
    short p1 = 5;
    short r1 = p1++;
    if (r1 != 5) return 1;
    if (p1 != 6) return 2;

    /* Test 2: prefix increment returns incremented value and variable updated */
    short p2 = 5;
    short r2 = ++p2;
    if (r2 != 6) return 3;
    if (p2 != 6) return 4;

    /* Test 3: postfix decrement returns original value, variable decremented afterwards */
    short p3 = 5;
    short r3 = p3--;
    if (r3 != 5) return 5;
    if (p3 != 4) return 6;

    /* Test 4: prefix decrement returns decremented value and variable updated */
    short p4 = 5;
    short r4 = --p4;
    if (r4 != 4) return 7;
    if (p4 != 4) return 8;

    /* Test 5: increments in larger expression; promotions happen before arithmetic */
    short a = 1000;
    short b = 2;
    short r5 = a++ + ++b; /* r5 = 1000 + 3 = 1003; a becomes 1001, b becomes 3 */
    if (r5 != 1003) return 9;
    if (a != 1001) return 10;
    if (b != 3) return 11;

    /* Test 6: decrements in larger expression with negative values */
    short c = -1000;
    short d = -2;
    short r6 = c-- + --d; /* r6 = -1000 + -3 = -1003; c becomes -1001, d becomes -3 */
    if (r6 != -1003) return 12;
    if (c != -1001) return 13;
    if (d != -3) return 14;

    /* Test 7: sequence of prefix and postfix on same variable in single expression:
       ensure well-defined evaluation order by avoiding undefined combinations; use parentheses
       to force separate subexpressions evaluated left-to-right in C. */
    short s1 = 1;
    short s2 = 1;
    short r7 = (s1++) + (++s2); /* r7 = 1 + 2 = 3; s1=2, s2=2 */
    if (r7 != 3) return 15;
    if (s1 != 2) return 16;
    if (s2 != 2) return 17;

    /* Test 8: check that applying unary plus yields promoted result after increment */
    short ip = 10;
    short r8 = +(++ip); /* r8 = 11, ip = 11 */
    if (r8 != 11) return 18;
    if (ip != 11) return 19;

    /* Test 9: ensure decrement results participate correctly in multiplication */
    short m1 = 200;
    short m2 = 3;
    short r9 = (--m1) * (m2--); /* r9 = 199 * 3 = 597; m1=199, m2=2 */
    if (r9 != 597) return 20;
    if (m1 != 199) return 21;
    if (m2 != 2) return 22;

    /* Test 10: ensure wrapping behavior at limits occurs after promotion (implementation-defined for overflow,
       but test that increments near SHRT_MAX change value appropriately without invoking undefined behavior here by staying below limit) */
    short hi = BC_SHRT_MAX - 1;
    short r10 = ++hi;
    if (r10 != BC_SHRT_MAX) return 23;
    if (hi != BC_SHRT_MAX) return 24;

    /* All tests passed */
    return 0;
}
