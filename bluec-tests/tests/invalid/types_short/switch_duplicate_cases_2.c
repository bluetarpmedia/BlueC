int switch_statement(int i) {
    switch((short) i) {
        case 100l: return 0;
        /* Even though 100l and 100 have different types, they have the same
         * value once converted to the type of the switch expression (int,
         * because of integer promotion), so they conflict
         */
        case 100: return 0;
        default: return 1;
    }
}

int main(void) {
    return switch_statement(100);
}