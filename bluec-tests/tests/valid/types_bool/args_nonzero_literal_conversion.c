_Bool ident(_Bool b) {
    return b;
}

int main(void) {
    if (ident('a') != 1) {
        return 1;
    }

    if (ident(1) != 1) {
        return 2;
    }

    if (ident(-1) != 1) {
        return 3;
    }

    if (ident(1024) != 1) {
        return 4;
    }

    if (ident(200U) != 1) {
        return 5;
    }
    
    if (ident(200L) != 1) {
        return 6;
    }

    if (ident(200ULL) != 1) {
        return 7;
    }

    if (ident(1.0f) != 1) {
        return 8;
    }

    if (ident(-1.0f) != 1) {
        return 9;
    }

    if (ident(1.0) != 1) {
        return 10;
    }

    if (ident(-1.0) != 1) {
        return 11;
    }

    if (ident("") != 1) {
        return 12;
    }

    if (ident("test") != 1) {
        return 13;
    }

    return 0;
}