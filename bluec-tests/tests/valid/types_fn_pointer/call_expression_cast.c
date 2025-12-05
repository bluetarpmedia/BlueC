int square(int x) {
    return x * x;
}

int main(void) {
    int a = ( ( int (*)(int) ) &square)(5);
    int b = ( ( int (*)(int) ) square)(6);

    if (a != 25) {
        return 1;
    }

    if (b != 36) {
        return 2;
    }

    return 0;
}