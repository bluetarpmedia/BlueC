/* Test long expressions in &&, ||, ! and controlling expressions */

int not(short s) {
    return !s;
}

int if_cond(short s) {
    if (s) {
        return s;
    }
    return 0;
}

int and(short s1, int l2) {
    return s1 && l2;
}

int or(int l1, short s2) {
    return l1 || s2;
}

int main(void) {
    // this would be equal to zero if we only considered lower 16 bits
    short s = 65535;
    short zero = 0;
    if (not(s)) {
        return 1;
    }
    if (!not(zero)) {
        return 2;
    }
    if(!if_cond(s)) {
        return 3;
    }
    if(if_cond(zero)) {
        return 4;
    }

    if (and(zero, 1)) {
        return 5;
    }

    if (!or(1, s)) {
        return 6;
    }

    return 0;
}