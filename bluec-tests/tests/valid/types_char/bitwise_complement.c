int main(void) {
    unsigned char val = 0xFF;
    
    // 1. 'val' should be promoted to int: 0x000000FF
    // 2. ~ is applied: 0xFFFFFF00
    // 3. 0xFFFFFF00 is NOT equal to 0
    if (~val == 0) {
        return 1;
    }

    return 0; // Success
}