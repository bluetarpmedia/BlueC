_Bool apply_and(_Bool a, _Bool b) {
    return a && b;
}

_Bool apply_or(_Bool a, _Bool b) {
    return a || b;
}

_Bool apply_not(_Bool a) {
    return !a;
}

int main(void) {
    if (apply_and(1, 1) != 1) {
        return 1;
    }
    
    if (apply_and(1, 0) != 0) {
        return 2;
    }

    if (apply_and(0, 0) != 0) {
        return 3;
    }

    if (apply_or(1, 1) != 1) {
        return 4;
    }

    if (apply_or(1, 0) != 1) {
        return 5;
    }

    if (apply_or(0, 1) != 1) {
        return 6;
    }

    if (apply_or(0, 0) != 0) {
        return 7;
    }

    if (apply_not(1) != 0) {
        return 8;
    }

    if (apply_not(0) != 1) {
        return 9;
    }

    return 0;
}