int main(void) {
    unsigned short val = 0xFFFF;
    
    // 1. 'val' should be promoted to int: 0x0000FFFF
    // 2. ~ is applied: 0xFFFF0000
    // 3. 0xFFFF0000 is NOT equal to 0
    if (~val == 0) {
        return 1;
    }

    return 0; // Success
}