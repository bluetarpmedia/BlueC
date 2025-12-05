int main(void) {
    int (*fn1)(void) = 0;
    int (*fn2)(void) = 0;
    
    int (*fn3)(void) = (1 > 2) ? fn1 : fn2;

    if (fn3 != fn2) {
        return 1;
    }

    return 0;
}