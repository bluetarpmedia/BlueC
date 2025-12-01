int main(void) {
    unsigned short a = 0;
    unsigned short b = (0);
    unsigned short c = (0 * 1);
    unsigned short d = 65535;
    unsigned short e = (65535);
    return a == b && b == c && d == e;
}