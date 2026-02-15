float sq(float val) {
    return val * val;
}

int main(void) {
    int a = 1;
    int b = 2;

    a = -b;
    a += 1 + 2 / 3;
    a = (b > 3) ? 1 : 2;

    float squared = sq(a);
    a = squared;
    b = (squared);

    float *ptr = &squared;
    float r1 = *ptr;
    float r2 = ptr[0];

    if ('a' > 'b') {
        return 'c';
    } else {
        return 'd';
    }

    if ('a' > 'b')
        return 'c';
    else
        return 'd';

    switch (a) {
        case 1:
            b = 2;
            break;
        case 2:
            b = 3;
            break;
        default:
            break;
    }

    while (a < 10) {
        a++;
    }

    do {
        b--;
    } while (b > 0);

    for (;;) {}

    for (int i = 0; i < 10; ++i) {
        a *= 2;
        break;
    }

    for (a = 0; a < 10; ++a) {
        b *= 2;
        continue;
    }

    goto exit;

exit:

    return 0;
}