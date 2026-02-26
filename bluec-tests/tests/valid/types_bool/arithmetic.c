int main(void) {
    _Bool b = 1;
    int sum = b + b + 10;
    if (sum != 12) {
        return 1;
    }

    b = 0;
    sum = b + b + 10;
    if (sum != 10) {
        return 2;
    }

    b = 1;
    b = b + 1;
    if (b != 1) {
        return 3;
    }

    b = 1;
    b = b - 1;
    if (b != 0) {
        return 4;
    }

    b = 1;
    b = b - 5;
    if (b != 1) {
        return 5;
    }

    b = 1;
    b = b * 10;
    if (b != 1) {
        return 6;
    }

    b = 1;
    b = b / 1;
    if (b != 1) {
        return 7;
    }

    return b * 100 - 1;
}