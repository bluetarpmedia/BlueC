int get(void) {
    return 5;
}

int main(void) {
    int (*fn1)(void) = get;
    int (**fn2)(void) = &fn1;
    int (***fn3)(void) = &fn2;

    int a = fn1();
    int b = (*fn1)();
    int c = (*fn2)();
    int d = (**fn2)();
    int e = (**fn3)();
    int f = (***fn3)();

    return a + b + c + d + e + f;
}