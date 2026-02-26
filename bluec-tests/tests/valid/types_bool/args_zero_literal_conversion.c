_Bool ident(_Bool b) {
    return b;
}

int main(void) {
    if (ident(0) != 0) {
        return 1;
    }

    if (ident(0L) != 0) {
        return 2;
    }

    if (ident(0U) != 0) {
        return 3;
    }

    if (ident(0UL) != 0) {
        return 4;
    }

    if (ident(0x0) != 0) {
        return 5;
    }

    if (ident(0.0f) != 0) {
        return 6;
    }

    if (ident(-0.0f) != 0) {
        return 7;
    }

    if (ident(0.0) != 0) {
        return 8;
    }

    if (ident(-0.0) != 0) {
        return 9;
    }

    return 0;
}