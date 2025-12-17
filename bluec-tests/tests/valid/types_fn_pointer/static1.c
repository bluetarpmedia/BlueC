int square(int x) {
    return x * x;
}

int (*fn1)(int) = square;
int (*fn2)(int) = &square;

int main(void) {
    int calc = fn1(2) + fn2(3);

    if (calc != 13) {
        return 1;
    }

    return 0;
}