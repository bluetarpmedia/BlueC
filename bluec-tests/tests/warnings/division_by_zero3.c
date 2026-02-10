int main(void) {
    char a = 'a';
    a /= 10 - 10;

    short b = (short)a / (short)0;

    int c = 0;
    c = (int)b % (15 - 15);

    return a + b + c;
}