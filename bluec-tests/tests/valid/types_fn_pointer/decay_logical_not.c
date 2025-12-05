short negate(short x) {
    return -x;
}

int main(void) {
    if (!negate) {
        return 1;
    }

    if (!&negate) {
        return 2;
    }

    int t1 = 0, t2 = 0;

    if (negate) {
        t1 = 1;
    }

    if (&negate) {
        t2 = 1;
    }

    if (t1 != 1) {
        return 3;
    }

    if (t2 != 1) {
        return 4;
    }

    return 0;
}