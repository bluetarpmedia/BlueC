int main(void) {
    short s = 1000;
    switch (s) {
        case 0:
            return 1;
        case 1000:
            return 0; // Success
        /* * This is not a duplicate of 1000 because 's' is promoted to 'int'.
         * If the switch expression were truncated to a 16-bit short, 
         * 66536 would wrap around (66536 % 65536) to 1000, 
         * causing a "duplicate case value" compiler error.
         */
        case 66536: 
            return 2;
        default:
            return 3;
    }
}