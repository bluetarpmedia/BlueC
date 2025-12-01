int main(void) {
    /* It's illegal to apply bitwise | to floats */
    long double f = 0.0L | -0.0L;
    return 0;
}