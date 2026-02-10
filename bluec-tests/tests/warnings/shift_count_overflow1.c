int main(void) {
    char  c = (char)(1 << 32);
    short s = (short)(1 << 32);
    int   i = 1 << 32;
    long  l = 1L << 64;

    c = 1 >> 40;
    s = 1 >> 40;
    i = 1 >> 40;
    l = 1L >> 80;

    return (int)(c + s + i + l);
}