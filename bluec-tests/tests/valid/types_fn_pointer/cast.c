long add(int a, int b) {
    return a + b;
}

int main(void) {
    long (*fn)(int, int) = 0;

    fn = (long (*)(int, int))add;

    if (fn(10, 20) != 30) {
        return 1;
    }

    fn = (long (*)(int, int))&add;

    if (fn(7, 3) != 10) {
        return 2;
    }

    return 0;
}