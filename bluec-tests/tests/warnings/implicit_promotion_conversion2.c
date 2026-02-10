int main(void) {
    char a = 1;
    char b = 2;
    a += -b;
    a += b + b;

    short x = 1;
    short y = 2;
    x += -y;
    x += y * a;

    return b + x;
}