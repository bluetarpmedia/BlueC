int main(void) {
    _Bool b1 = 0.1;
    _Bool b2 = 0.7f;
    _Bool b3 = 1024;
    _Bool b4 = 0.0;
    _Bool b5 = -0.0;

    if (b1 != 1) {
        return 1;
    }

    if (b2 != 1) {
        return 2;
    }

    if (b3 != 1) {
        return 3;
    }

    if (b4 != 0) {
        return 4;
    }

    if (b5 != 0) {
        return 4;
    }

    return 0;
}
