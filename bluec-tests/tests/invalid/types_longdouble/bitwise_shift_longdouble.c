int main(void) {
    /* It's illegal to apply the << or >> operator to floats */
    long double f = 5.0L << 3;
    return 0;
}