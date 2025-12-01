#ifdef SUPPRESS_WARNINGS
#pragma GCC diagnostic ignored "-Wunused-parameter"
#endif
int test_sum(int a, int b, int c, long long d, int e, long long f, int g, int h, long long i) {
    /* Make sure the arguments passed in main weren't converted to ints */
    if (d + f < 100LL) {
        return 1;
    }
    /* Check an argument that was passed on the stack too */
    if (i < 100LL)
        return 2;
    return 0;
}