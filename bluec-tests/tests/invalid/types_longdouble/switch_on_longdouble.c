int main(void) {
    long double f = 10.0L;
    /* The controlling expression in a switch statement
     * must be an integer, not a float
     */
    switch (f) {
        case 10: return 0;
    }
    return 1;
}