int main(void) {
    char  c = 1;
    short s = 1;
    int   i = 1;
    long  l = 1;

    c <<= 32;
    c >>= 33;

    s <<= 32;
    s >>= 33;

    i >>= 32;
    l >>= 64;

    return (int)(c + s + i + l);
}