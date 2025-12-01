short return_truncated_long(long l) {
    return l;
}

long return_extended_int(short s) {
    return s;
}

int truncate_on_assignment(long l, short expected) {
    short result = l; // implicit conversion truncates l
    return result == expected;
}

int main(void) {

    // return statements

    /* return_truncated_long will truncate 2^32 + 2 to 2
     * assigning it to result converts this to a long
     * but preserves its value.
     */
    long result = return_truncated_long(4294967298l);
    if (result != 2l) {
        return 1;
    }

    /* return_extended_int sign-extends its argument, preserving its value */
    result = return_extended_int(-10);
    if (result != -10) {
        return 2;
    }

    // initializer

    /* This is 2^32 + 2,
     * it will be truncated to 2 by assignment
     */
    int i = 4294967298l;
    if (i != 2) {
        return 3;
    }

    // assignment expression

    // 2^34 will be truncated to 0 when assigned to an int
    if (!truncate_on_assignment(17179869184l, 0)) {
        return 4;
    }

    return 0;
}