int get1(void) {
    return 1;
}

int get2(void) {
    return 2;
}

int main(void) {
    int v = ((1 > 2) ? get1 : get2)();

    if (v != 2) {
        return 1;
    }

    return 0;
}