int main(void) {
    int a = 1, b = 2, c = 3;

    int x = a + b << c;
    int y = a << b + c;

    return x + y;
}