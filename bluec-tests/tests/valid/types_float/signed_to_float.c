/* Test conversions from signed integer types to float */
float short_to_float(short s) {
    return (float) s;
}

float int_to_float(int i) {
    return (float) i;
}

float long_to_float(long l) {
    return (float) l;
}
int main(void) {

    if (short_to_float(-100) != -100.0f) {
        return 1;
    }

    if (int_to_float(-100000) != -100000.0f) {
        return 2;
    }

    if (long_to_float(-9007199254751227l) != -9007199254751228.0f) {
        return 3;
    }

    // cast a constant to float to exercise rewrite rule for cvtsi2ss $const, dst
    float f = (float) 1152921504606846977l; // 2^60 + 1; nearest float is 2^60
    if (f != 1152921504606846976.0f) {
        return 4;
    }

    return 0;
}