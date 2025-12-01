/* Test conversions from float to unsigned integer types */

unsigned int float_to_uint(float d) {
    return (unsigned int) d;
}

unsigned long float_to_ulong(float d) {
    return (unsigned long) d;
}

int main(void) {

    // try a float in the range of signed int;
    if (float_to_uint(10.9f) != 10u) {
        return 1;
    }

    // now try a float in the range of unsigned int but not of int
    if (float_to_uint(2147483648.5f) != 2147483648) {
        return 2;
    }

    // convert a float within the range of signed long,
    // so cvttss2siq is already correct
    if (float_to_ulong(34359738368.5f) != 34359738368ul) {
        return 3;
    }

    // now convert a float that's larger than LONG_MAX,
    // so we need to correct the results of cvttss2siq
    if (float_to_ulong(9223372036854775808.5f) != 9223372036854775808ul) {
        return 4;
    }

    return 0;

}