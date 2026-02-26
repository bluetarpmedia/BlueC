_Bool ident(_Bool b) {
    return b;
}

int main(void) {
    int i = 1;
    if (ident(i) != 1) {
        return 1;
    }

    i = 0;
    if (ident(i) != 0) {
        return 2;
    }

    i = 1111;
    if (ident(i) != 1) {
        return 3;
    }

    double d = 0.0;
    if (ident(d) != 0) {
        return 4;
    }

    d = -0.0;
    if (ident(d) != 0) {
        return 5;
    }

    d = 1.0;
    if (ident(d) != 1) {
        return 6;
    }

    d = -1.0;
    if (ident(d) != 1) {
        return 7;
    }

    char c = 'a';
    if (ident(c) != 1) {
        return 8;
    }

    char text[5] = "test";
    if (ident(text) != 1) {
        return 9;
    }

    return 0;
}