int main(void) {
    unsigned long a = 0;
    unsigned long b = (0);
    unsigned long c = (0 * 1);
    unsigned long d = 18446744073709551615 + 1;
    unsigned long e = (18446744073709551615 + 1);
    return a == b && b == c && d == e;
}