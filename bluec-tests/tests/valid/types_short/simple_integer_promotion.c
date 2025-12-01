
/* Test that signed short operands are promoted to int before addition.
   If the compiler performs the addition in 16 bits and then sign-extends
   the 16-bit result to int, the test will fail. */
int main(void) {
    short a = 30000;
    short b = 30000;
    int s = a + b;           /* must be performed in int (30000 + 30000 == 60000) */
    return s == 60000;       /* return 1 if promotions occurred correctly, 0 otherwise */
}