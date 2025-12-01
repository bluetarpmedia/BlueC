int main(void) {
    /* It's illegal to apply bitwise & to floats */
    long double f = 10.0L & -1;
    return 0;
}