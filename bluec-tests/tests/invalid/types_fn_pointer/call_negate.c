int get10(void) {
    return 10;
}

int main(void) {
    int (*fn)(void) = get10;
    int v = (-fn)();
    return v;
}