int main(void) {
    _Bool arr[8] = {-1, +1, !1, ~1, -0, +0, !0, ~0};

    if (arr[0] != 1) {
        return 1;
    }

    if (arr[1] != 1) {
        return 2;
    }

    if (arr[2] != 0) {
        return 3;
    }

    if (arr[3] != 1) {
        return 4;
    }

    if (arr[4] != 0) {
        return 5;
    }

    if (arr[5] != 0) {
        return 6;
    }

    if (arr[6] != 1) {
        return 7;
    }

    if (arr[7] != 1) {
        return 8;
    }

    return 0;
}