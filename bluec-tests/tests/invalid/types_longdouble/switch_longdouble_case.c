int main(void) {
    int x = 10;
    switch (x) {
        // the constant in a case expression
        // must be an integer, not a float
        case 1.0L: return 0;
        default: return 4;
    }
}