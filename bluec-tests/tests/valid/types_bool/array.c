typedef unsigned long size_t;
void *memset(void *s, int c, size_t n);

int main(void) {
    _Bool flags[5] = {1, 0, 1}; // Rest should be 0
    if (flags[0] != 1) return 1;
    if (flags[1] != 0) return 2;
    if (flags[3] != 0) return 3;
    if (flags[4] != 0) return 4;

    flags[1] = 255;
    if (flags[1] != 1) return 5;

    _Bool *ptr = &flags[0];
    if (*(ptr + 2) != 1) return 6;

    if (sizeof(flags) != (5 * sizeof(_Bool))) return 7;

    memset(flags, 0, sizeof(flags));
    if (flags[0] != 0 || flags[4] != 0) return 8;

    _Bool matrix[2][2] = { {1, 0}, {0, 1} };
    if (matrix[1][1] != 1) return 9;
    if (matrix[0][1] != 0) return 10;

    flags[0] -= 1;
    if (flags[0] != 1) return 11;

    if (!flags) return 12;

    return 0;
}