int get10(void) {
    return 10;
}

int main(void) {
    int (*fn)(void) = get10;
    int v = (0 + fn)();

    if (v != 10) {
        return 1;
    }

    return 0;
}