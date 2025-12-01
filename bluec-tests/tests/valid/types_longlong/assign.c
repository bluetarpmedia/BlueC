int main(void) {
    /* initializing a tests the rewrite rule for
     * movq $large_const, memory_address
     */
    long long a = 4294967290LL;
    long long b = 0LL;
    /* Assign the value of one long variable
     * (which is too large for an int to represent)
     * to another long variable
     */
    b = a;
    return (b == 4294967290LL);
}