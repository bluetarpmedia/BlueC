int main(void) {
    float f = 0.0f;
    double d = 0.0;
    long double ld = 0.0;

    f  += 1e40f;
    d  += 1e400;
    ld += 1e400L;

    f  = 0x1p150f;
    d  = 0x1p2000;
    ld = 0x1p2000L;

    return 0;
}