int main(void) {
    char  c = 0;
    short s = 0;
    int   i = 0;
    long  l = 0;

    char c1 = 1;
    short s1 = 1;
    int i1 = 1;
    long l1 = 1L;

    c = (char)(c1 << 32);
    s = (short)(s1 << 32);
    i = i1 << 32;
    l = l1 << 64;

    return (int)(c + s + i + l);
}