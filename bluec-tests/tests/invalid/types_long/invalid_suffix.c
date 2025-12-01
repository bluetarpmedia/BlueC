int main(void) {
    /* only one "L" suffix is permitted on a long
     * Note: an "LL" suffix is standard-compliant and indicates
     * a long long constant but "lL" and "Ll" suffixes are invalid
     */
    return 0lL + 0Ll;
}