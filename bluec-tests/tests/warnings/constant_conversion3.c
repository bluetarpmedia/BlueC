int main(void) {
    int a = 4294967196;
    int b = 4294967196u;
    unsigned c = -1;
    unsigned d = -4294967296;
    unsigned short e = 9223372036854775808ull;
    return a == -100 && b == -100 && c == 0xFFFFFFFF && d == 0 && e == 0;
}