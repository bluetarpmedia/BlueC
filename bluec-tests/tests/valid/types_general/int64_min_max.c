#define BC_LONG_MIN1 (-9223372036854775807 - 1)
#define BC_LONG_MIN2 (-9223372036854775807L - 1L)

int test1(void) {
    // Without suffixes
    long a = BC_LONG_MIN1;
    long b = (BC_LONG_MIN1);
    long c = (9223372036854775807 * -1) - 1;
    long d = 9223372036854775807;
    long e = (9223372036854775807);
    return a == b && b == c && d == e;
}

int test2(void) {
    // With suffixes
    long a = BC_LONG_MIN2;
    long b = (BC_LONG_MIN2);
    long c = (9223372036854775807L * -1L) - 1L;
    long d = 9223372036854775807L;
    long e = (9223372036854775807L);
    return a == b && b == c && d == e;
}

int main(void) {
    if (test1() != 1) {
        return 1;
    }

    if (test2() != 1) {
        return 2;
    }

    return 0;
}