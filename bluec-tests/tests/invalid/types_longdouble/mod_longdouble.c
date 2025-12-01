int main(void) {
    /* You can't apply the modulo operator to a float */
    long double f = 10.0L;
    f = f % 3;
    return 0;
}