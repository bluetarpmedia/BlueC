

/* A recursive function in which both float and integer parameters
 * are passed in registers and on the stack */
int fun(int i1, float f1, int i2, float f2, int i3, float f3,
        int i4, float f4, int i5, float f5, int i6, float f6,
        int i7, float f7, int i8, float f8, int i9, float f9) {


    if (i1 != f9) {
        /* make two recursive calls that bring these values closer together:
         * 1. increment i1 and all ints: */
        int call1 = fun(i1 + 1, f1, i2 + 1, f2, i3 + 1, f3, i4 + 1, f4, i5 + 1, f5, i6 + 1, f6, i7 + 1, f7, i8 + 1, f8, i9 + 1, f9);

        /* 2. decrement f9 and all floats */
        int call2 = fun(i1, f1 - 1, i2, f2 - 1, i3, f3 - 1, i4, f4 - 1, i5, f5 - 1, i6, f6 - 1, i7, f7 - 1, i8, f8 - 1, i9, f9 - 1);

        /* Make sure both calls succeeded; non-zero result indicates a problem */
        if (call1) {
            return call1;
        }

        if (call2) {
            return call2;
        }

    }

    // make sure all arguments have expected value; value of each arg relative to i1 (for ints)
    // or f9 (for floats) should stay the same. this ensures that we preserve value of function parameters
    // even across other function calls
    if (i2 != i1 + 2) {
        return 2;
    }
    if (i3 != i1 + 4) {
        return 3;
    }
    if (i4 != i1 + 6) {
        return 4;
    }
    if (i5 != i1 + 8) {
        return 5;
    }
    if (i6 != i1 + 10) {
        return 6;
    }
    if (i7 != i1 + 12) {
        return 7;
    }
    if (i8 != i1 + 14) {
        return 8;
    }
    if (i9 != i1 + 16) {
        return 9;
    }
    if (f1 != f9 - 16) {
        return  11;
    }
    if (f2 != f9 - 14) {
        return  12;
    }
    if (f3 != f9 - 12) {
        return  13;
    }
    if (f4 != f9 - 10) {
        return  14;
    }
    if (f5 != f9 - 8) {
        return  15;
    }
    if (f6 != f9 - 6) {
        return  16;
    }
    if (f7 != f9 - 4) {
        return  17;
    }
    if (f8 != f9 - 2) {
        return  18;
    }

    return 0;
}

int main(void) {
    return fun(1, 2.0f, 3, 4.0f, 5, 6.0f, 7, 8.0f, 9, 10.0f, 11, 12.0f, 13, 14.0f, 15, 16.0f, 17, 18.0f);
}