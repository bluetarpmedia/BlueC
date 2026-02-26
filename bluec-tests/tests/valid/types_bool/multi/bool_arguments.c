int check_args(_Bool a, _Bool b, _Bool c, _Bool d, _Bool e, _Bool f, _Bool g, _Bool h) {
    _Bool expected_a = 1;
    _Bool expected_b = 0;
    _Bool expected_c = 1;
    _Bool expected_d = 0;
    _Bool expected_e = 1;
    _Bool expected_f = 0;
    _Bool expected_g = 1;
    _Bool expected_h = 0;

    if (expected_a != a) {
        return 1;
    }

    if (expected_b != b) {
        return 2;
    }

    if (expected_c != c) {
        return 3;
    }

    if (expected_d != d) {
        return 4;
    }

    if (expected_e != e) {
        return 5;
    }

    if (expected_f != f) {
        return 6;
    }

    if (expected_g != g) {
        return 7;
    }

    if (expected_h != h) {
        return 8;
    }

    return 0;
}
