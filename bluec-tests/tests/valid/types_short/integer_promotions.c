/* Return 0 on success (promotions behaved correctly), non-zero on failure.
   Uses values that would produce different results if operations were
   performed in 16-bit arithmetic instead of after promotion to int. */
int main(void) {
    short a = 30000;          /* fits in int but not in 16-bit signed arithmetic without wrap */
    short b = 2;
    short sneg = -12345;

    /* Arithmetic */
    int t_add = a + a;        /* expected 60000 */
    if (t_add != 60000) return 1;

    int t_mul = a * b;        /* expected 60000 */
    if (t_mul != 60000) return 2;

    int t_sub = a - sneg;     /* expected 42345 (30000 - (-12345)) */
    if (t_sub != 42345) return 3;

    int t_div = a / b;        /* expected 15000 */
    if (t_div != 15000) return 4;

    int t_mod = a % b;        /* expected 0 */
    if (t_mod != 0) return 5;

    /* Unary */
    int t_neg = -a;           /* expected -30000 */
    if (t_neg != -30000) return 6;

    int t_compl = ~a;         /* expected bitwise inverse of 30000 in int */
    if (t_compl != (~(int)30000)) return 7;

    /* Bitwise */
    int t_and = a & 65535;   /* expected 30000 */
    if (t_and != 30000) return 8;

    int t_or = a | 458752;   /* expected (30000 | 0x70000) */
    if (t_or != ((int)30000 | 458752)) return 9;

    int t_xor = a ^ -65536; /* expected ((int)30000 ^ 0xFFFF0000) */
    if (t_xor != ((int)30000 ^ -65536)) return 10;

    /* Shifts: both operands are promoted; left-shift stays within int range */
    int t_shl = a << 1;       /* expected 60000 */
    if (t_shl != 60000) return 11;

    /* Comparisons (use results as ints 0 or 1) */
    if ((a > 10000) != 1) return 12;
    if ((sneg < 0) != 1) return 13;
    if ((a != sneg) != 1) return 14;
    if (!(a == sneg) != 1) return 15;

    /* Logical operators (operands promoted, result 0 or 1) */
    if ((a && 0) != 0) return 16;
    if ((a && 1) != 1) return 17;
    if (((short)0 || a) != 1) return 18;

    /* Ternary */
    if (((a == 30000) ? b : 458752) != 2) return 19;
    if (((a != 30000) ? b : 458752) != 458752) return 20;

    return 0;
}
