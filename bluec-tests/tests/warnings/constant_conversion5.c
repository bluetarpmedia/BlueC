int main(void) {
    int a = 0;
    int b = 1;
    unsigned c = 0;
    unsigned short d = 0;
    unsigned char e = 0;

    a += 4294967195 + 1;
    b *= 4294967195u + 1u;

    c = 9 - 10;
    d = -4294967296 / 1;
    e += 9223372036854775808ull;

    return a == -100 && b == -100 && c == 0xFFFFFFFF && d == 0 && e == 0;
}