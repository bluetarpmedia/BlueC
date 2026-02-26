int main(void) {
    _Bool t = 0b1;
    _Bool f = 0b0;

    if (t == f) {
        return 1;
    }

    if (t != 1) {
        return 2;
    }

    if (f != 0) {
        return 3;
    }

    return 0;
}