int main(void) {
    _Bool b = 1;

    // No warnings expected
    if (b == 0) return 0;
    if (b == 1) return 0;
    if (b > 0) return 0;
    if (b < 1) return 0;
    if (b <= 0) return 0;
    if (b >= 1) return 0;

    // Warnings expected
    if (b == -1) return 1;
    if (b == 2) return 1;
    if (b != -1) return 1;
    if (b != 2) return 1;

    if (b < 0) return 2;
    if (b > 1) return 2;
    if (b < 2) return 2;
    if (b > -1) return 2;

    if (b <= -1) return 3;
    if (b >= 2) return 3;

    if (b >= -1) return 4;
    if (b >= 0) return 4;
    if (b <= 1) return 4;
    if (b <= 2) return 4;

    return 0;
}