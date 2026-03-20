int main(void) {
    int a = 1, b = 2;

    _Bool r1 = a << 1;

    _Bool r2 = a || b << 2;

    if (a * b) {
        return 1;
    }

    return r1 + r2;
}