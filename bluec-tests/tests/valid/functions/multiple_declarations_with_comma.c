int main(void) {
    int calc1(int a, int b), calc2(int a, int b);
    int sum = calc1(1, 2) + calc2(3, 4);
    if (sum != 10)
        return 1;

    return 0;
}

int calc1(int a, int b) {
    return a + b;
}

int calc2(int a, int b) {
    return a + b;
}
