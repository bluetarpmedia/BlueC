int switch_statement(int i) {
    switch((long long) i) {
        case 100LL: return 0;
        /* Even though 100l and 100 have different types, they have the same
         * value once converted to the type of the switch expression (long long)
         * so they conflict
         */
        case 100: return 0;
        default: return 1;
    }
}

int main(void) {
    return switch_statement(100);
}