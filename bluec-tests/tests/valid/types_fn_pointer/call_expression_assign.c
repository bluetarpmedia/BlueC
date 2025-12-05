int get10(void) {
    return 10;
}

int get20(void) {
    return 20;
}

int main(void) {
    int (*fn)(void) = 0;
    int v = (fn = get20)();

    if (v != 20) {
        return 1;
    }

    return 0;
}