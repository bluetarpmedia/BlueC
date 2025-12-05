int get(void) {
    return 11;
}

static int next = 1;
static int get_next(void) {
    return next++;
}

int (*g1)(void);
int (*g2)(void);

int main(void) {
    int (*p1)(void) = 0;
    int (*p2)(void) = 0;

    if (g1 != g2) {
        return 1;
    }

    if (p1 != p2) {
        return 2;
    }

    if (g1 != 0) {
        return 3;
    }

    if (p1 != 0) {
        return 4;
    }

    g1 = get;
    g2 = &get;

    p1 = get_next;
    p2 = &get_next;

    if (g1 != g2) {
        return 5;
    }

    if (p1 != p2) {
        return 6;
    }

    p1();
    p2();
    p1();
    p2();

    int value = p1();
    if (value != 5) {
        return 7;
    }

    p1 = 0;
    if (p1 == p2) {
        return 8;
    }

    g2 = 0;
    if (g1 == g2) {
        return 9;
    }

    int (**p3)(void) = 0;
    if (p3 != p1) {
        return 10;
    }

    p3 = &p2;
    if (p3 == p2) {
        return 11;
    }

    if (*p3 != p2) {
        return 12;
    }

    return 0;
}