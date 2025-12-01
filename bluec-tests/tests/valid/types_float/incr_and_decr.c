// test ++/-- with floats
int main(void) {
    static float f = 0.75f;
    // basic tests
    if (f++ != 0.75f) {
        return 1;
    }
    if (f != 1.75f) {
        return 2;
    }

    f = -100.2f;
    if (++f != -99.2f) {
        return 3;
    }
    if (f != -99.2f) {
        return 4;
    }

    if (f-- != -99.2f) {
        return 5;
    }
    if (f != -100.2f) {
        return 6;
    }

    if (--f != -101.2f) {
        return 7;
    }
    if (f != -101.2f) {
        return 8;
    }

    // if initial value is very small, it may disappear due to rounding error
    // after incr/decr
    f = 0.000000000000000000001f;
    f++;
    if (f != 1.0) {
        return 9;
    }

    // ++ and -- have no effect if gap between representable values is greater
    // than 1
    f = 1e10;
    f--;
    if (f != 1e10) {
        return 10;
    }
    return 0;
}