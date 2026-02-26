int main(void) {
    _Bool t = 1;
    _Bool f = 0;
    _Bool res;

    if (!t != 0) return 1;
    if (!f != 1) return 2;

    res = ~t;
    if (res != 1) return 3;
    res = ~f;
    if (res != 1) return 4;

    res = -t; 
    if (res != 1) return 5;
    res = -f;
    if (res != 0) return 6;

    res = +t;
    if (res != 1) return 7;

    res = 0;
    if (++res != 1) return 8;
    if (++res != 1) return 9; // 1 -> 2 -> 1

    res = 0;
    _Bool old = res++;
    if (old != 0 || res != 1) return 10;

    res = 1;
    if (--res != 0) return 11;
    if (--res != 1) return 12; // 0 -> -1 -> 1

    if (sizeof(t) < 1) return 13;

    _Bool *ptr = &t;
    if (*ptr != 1) return 14;

    if ((_Bool)123 != 1) return 15;

    return 0;
}