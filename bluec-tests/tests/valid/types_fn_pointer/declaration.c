int square(int x) {
    return x * x;
}

int apply(int (*)(int), int);

int main(void) {
    int result = apply(square, 5);
    
    if (result != 25) {
        return 1;
    }

    return 0;
}

int apply(int (*fn)(int), int value) {
    return fn(value);
}