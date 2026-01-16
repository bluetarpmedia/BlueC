int main(void) {
    char a = 'A';
    signed char b = 'A';
    unsigned char c = 'A';

    double d1 = a;
    double d2 = b;
    double d3 = c;

    float f1 = a;
    float f2 = b;
    float f3 = c;

    char double_valid = d1 == d2 && d2 == d3 && d1 == 65.0;
    char float_valid = f1 == f2 && f2 == f3 && f1 == 65.0f;

    if (!double_valid) {
        return 1;
    }

    if (!float_valid) {
        return 2;
    }

    return 0;
}