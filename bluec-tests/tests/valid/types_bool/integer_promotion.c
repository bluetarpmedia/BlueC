/* Test that we promote _Bool to integer when we're required to */

int add_bools(_Bool b1, _Bool b2, _Bool b3) {
    return b1 + b2 + b3;
}

int negate(_Bool b) {
    return -b;
}

int complement(_Bool b) {
    return ~b;
}

int add_then_div(_Bool a, _Bool b, _Bool c) {
    return (a + b) / c;
}

_Bool decrement(_Bool b) {
    b = b - 1;
    return b;
}

int main(void) {
    _Bool a = 1;
    _Bool b = 1;

    if (add_bools(a, a, b) != 3) {
        return 1;
    }

    _Bool one = 1;
    if (negate(one) != -1) {
        return 2;
    }

    if (complement(one) != -2) {
        return 3;
    }

    _Bool w = 1;
    _Bool x = 1;
    _Bool y = 1;
    if (add_then_div(w, x, y) != 2)
        return 4;

    if (decrement(w) != 0) {
        return 7;
    }

    return 0;
}
