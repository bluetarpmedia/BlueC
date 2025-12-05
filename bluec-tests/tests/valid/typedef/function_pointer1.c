typedef int BinaryFunc(int, int);

int do_work(BinaryFunc *binary_fn, int x, int y) {
    return binary_fn(x, y);
}

int Plus(int a, int b) {
    return a + b;
}

int Mul(int a, int b) {
    return a * b;
}

int main(void) {
    int a = do_work(Plus, 5, 6);
    int b = do_work(Mul, a, 2);
    return b;
}
