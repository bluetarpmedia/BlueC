int get1(void) {
    return 1;
}

int get2(void) {
    return 2;
}

int main(void) {
    int (*fn)(void) = (1 > 2) ? get1 : get2;

    if (fn() != 2) {
        return 1;
    }

    return 0;
}