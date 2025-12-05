short negate(short x) {
    return -x;
}

int main(void) {
    short (*fn)(short) = 0;

    if (fn) {
        return 1;
    }

    fn = negate;

    if (!fn) {
        return 2;
    }

    if (fn(5) != -5) {
        return 3;
    }

    return 0;
}