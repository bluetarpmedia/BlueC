/* Test conversions from float to the signed integer types */

int float_to_int(float d) {
    return (int) d;
}

long float_to_long(float d) {
    return (long) d;
}

int main(void) {

    long l = float_to_long(16777210.3f);
    // should be truncated towards 0
    if (l != 16777210l) {
        return 1;
    }

    int i = float_to_int(-20000.9f);
    // negative value should be truncated towards 0
    if (i != -20000) {
        return 2;
    }

    return 0;
}