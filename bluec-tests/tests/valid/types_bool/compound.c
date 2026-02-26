int main(void) {
    _Bool b;

    b = 1;
    b += 1;
    if (b != 1) return 1;

    // 0 - 1 == -1, and -1 is nonzero so becomes 'true'.
    b = 0;
    b -= 1;
    if (b != 1) return 2;

    b = 1;
    b *= 5;
    if (b != 1) return 3;
    
    b *= 0;
    if (b != 0) return 4;

    b = 0;
    b |= 2;
    if (b != 1) return 5;

    b = 1;
    b ^= 1;
    if (b != 0) return 6;
    
    b ^= 2;
    if (b != 1) return 7;

    b = 1;
    b <<= 1;
    if (b != 1) return 8;

    b = 1;
    b >>= 1;
    if (b != 0) return 9;

    b = 1;
    b /= 2;
    if (b != 0) return 10;

    return 0;
}