/* Test conversions from unsigned integer types to floats */
float ushort_to_float(unsigned short us) {
    return (float) us;
}

float uint_to_float(unsigned int ui) {
    return (float) ui;
}

float ulong_to_float(unsigned long ul) {
    return (float) ul;
}

int main(void) {

    // unsigned short
    if (ushort_to_float(30000) != 30000.0f) {
        return 1;
    }

    // uint that's smaller than INT_MAX
    if (uint_to_float(1000u) != 1000.0f) {
        return 2;
    }

    // uint that's larger than INT_MAX, so we can't just use cvtsi2sd
    if (uint_to_float(4294967200u) != 4294967200.0f) {
        return 3;
    }

    // ulong that's smaller than LONG_MAX
    if (ulong_to_float(138512825844ul) != 138512825844.0f) {
        return 4;
    }

    // ulong that's larger than LONG_MAX
    if (ulong_to_float(10223372036854775816ul) != 10223372036854775808.0f) {
        return 5;
    }

    /* To test our rounding-to-odd implementation, use the unsigned long from
     * "Converting an Unsigned Integer to a float" in Chapter 13
     * and other values illustrates in Figure 13-7
     */

    /* This value is exactly between 9223372036854775808.0 and 9223372036854777856.0
     * Using ties-to-even rounding, we'll round it down to
     * 9223372036854775808.0, which has an even significand
     */
    if (ulong_to_float(9223372036854776832ul) != 9223372036854775808.0f) {
        return 6;
    }

    /* This value is closer to 9223372036854777856.0 than 9223372036854775808.0,
     * so we should round up.
     */
    if (ulong_to_float(9223372036854776833ul) != 9223372036854777856.0f) {
        return 7;
    }

    /* This value is closer to 9223372036854775808.0 than 9223372036854777856.0,
     * so round down */
    if (ulong_to_float(9223372036854776831ul) != 9223372036854775808.0f) {
        return 8;
    }

    /* This value is closer to 9223372036854775808.0 than 9223372036854777856.0,
     * so round down */
    if (ulong_to_float(9223372036854776830ul) != 9223372036854775808.0f) {
        return 9;
    }

    return 0;
}