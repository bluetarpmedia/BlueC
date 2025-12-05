int calc(int (*binary_fn)(int, int), int x, int y) {
    return binary_fn(x, y);
}

int Plus(int a, int b) {
    return a + b;
}

int Mul(int a, int b) {
    return a * b;
}

int main(void) {
    int a = calc(Plus, 100, 50);
    int b = calc(Mul, a, -1);
    int c = calc(&Plus, a, b);
    int d = calc(&Mul, b, -1);

    if (a != 150) {
        return 1;
    }

    if (b != -150) {
        return 2;
    }

    if (c != 0) {
        return 3;
    }

    if (d != 150) {
        return 4;
    }

    return 0;
}
