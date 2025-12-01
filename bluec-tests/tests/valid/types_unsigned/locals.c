int main(void) {
    /* Initialize and then update a mix of variables,
     * to check that we allocate enough stack space for each of them,
     * and writing to one doesn't clobber another.
     * Identical to chapter 11 long_and_int_locals but with some unsigned integers
     */

    unsigned long a = 8589934592ul; // this number is outside the range of int
    int b = -1;
    unsigned short f = 12;
    long c = -8589934592l; // also outside the range of int
    unsigned int d = 10u;
    unsigned short e = 11;

    /* Make sure every variable has the right value */
    if (a != 8589934592ul) {
        return 1;
    }
    if (b != -1){
        return 2;
    }
    if (c != -8589934592l) {
        return 3;
    }
    if (d != 10u) {
        return 4;
    }
    if (e != 11) {
        return 5;
    }
    if (f != 12) {
        return 6;
    }

    /* update every variable */
    a = -a;
    b = b - 1;
    c = c + 8589934594l;
    d = d * 268435456u; // result is between INT_MAX and UINT_MAX
    e = e + 1;
    f = f - 1;

    /* Make sure the updated values are correct */
    if (a != 18446744065119617024ul) {
        return 7;
    }
    if (b != -2) {
        return 8;
    }
    if (c != 2) {
        return 9;
    }
    if (d != 2684354560u) {
        return 10;
    }
    if (e != 12) {
        return 11;
    }
    if (f != 11) {
        return 12;
    }

    return 0;
}