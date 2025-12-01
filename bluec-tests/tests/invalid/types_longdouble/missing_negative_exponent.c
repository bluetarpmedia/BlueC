int main(void) {
    /* The exponent on a floating-point constant must be an integer,
     * not just a negative sign.
     */
    long double foo = 24e-L;
}