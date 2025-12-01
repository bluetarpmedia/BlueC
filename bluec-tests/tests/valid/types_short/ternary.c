int main(void) {
    /* Test 1: selecting a short operand returns its value (promoted to int) */
    short a1 = 30000;
    short b1 = 123;
    if ((1 ? a1 : b1) != 30000) return 1;

    /* Test 2: selecting an integer literal when condition false */
    short s2 = 5;
    if ((0 ? s2 : 100) != 100) return 2;

    /* Test 3: sign preservation when selecting a negative short */
    short s3 = -12345;
    if ((1 ? s3 : 0) != -12345) return 3;

    /* Test 4: result uses usual arithmetic conversions when branches differ in rank;
       a short branch vs an int literal branch should yield the int literal value */
    short s4 = 30000;
    if ((0 ? s4 : 100000) != 100000) return 4;

    /* Test 5: only the chosen branch is evaluated (side effect test) */
    short x5 = 0;
    short y5 = 0;
    int r5 = (1 ? ++x5 : ++y5);
    if (r5 != 1) return 5;
    if (x5 != 1) return 6;
    if (y5 != 0) return 7;

    return 0;
}
