/* Test conversions between signed and unsigned types of the same size */

int ushort_to_short(unsigned short us, short expected) {
    return (short) us == expected;
}

int short_to_ushort(short s, unsigned short expected) {
    return (unsigned short) s == expected;
}

int uint_to_int(unsigned int ui, int expected) {
    return (int) ui == expected;
}

int int_to_uint(int i, unsigned int expected) {
    return (unsigned int) i == expected;
}

int ulong_to_long(unsigned long ul, signed long expected) {
    return (signed long) ul == expected;
}

int long_to_ulong(long l, unsigned long expected) {
    return (unsigned long) l == expected;
}

int main(void) {

    /* Converting a positive signed int to an unsigned int preserves its value */
    if (!int_to_uint(10, 10u)) {
        return 1;
    }

    /* If an unsigned int is within the range of signed int,
     * converting it to a signed int preserves its value
     */
    if (!uint_to_int(10u, 10)) {
        return 2;
    }

    /* Converting a negative signed long -x to an unsigned long
     * results in 2^64 - x
     */
    if (!long_to_ulong(-1000l, 18446744073709550616ul)) {
        return 3;
    }

    /* If an unsigned long is too large for a long to represent,
     * reduce it modulo 2^64 until it's in range.
     */
    if (!ulong_to_long(18446744073709550616ul, -1000l)) {
        return 4;
    }

    /* Converting a positive signed short to an unsigned short preserves its value */
    if (!short_to_ushort(10, 10)) {
        return 5;
    }

    /* If an unsigned short is within the range of signed short,
     * converting it to a signed short preserves its value
     */
    if (!ushort_to_short(10u, 10)) {
        return 6;
    }

    return 0;
}