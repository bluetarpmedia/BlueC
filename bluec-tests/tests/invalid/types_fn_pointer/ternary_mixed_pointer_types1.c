int main(void) {
    float (*fn1)(void) = 0;
    int (*fn2)(void) = 0;
    
    int (*fn3)(void) = (1 > 2) ? fn1 : fn2;

    return 0;
}