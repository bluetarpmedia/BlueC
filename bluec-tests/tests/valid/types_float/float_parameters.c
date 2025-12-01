/* Test that we've properly implemented the calling convention for float parameters passed in registers */
int check_arguments(float a, float b, float c, float d, float e, float f, float g, float h);

int main(void) {
    return check_arguments(1.0f, 2.0f, 3.0f, 4.0f, -1.0f, -2.0f, -3.0f, -4.0f);
}

int check_arguments(float a, float b, float c, float d, float e, float f, float g, float h) {
    if (a != 1.0f) {
        return 1;
    }
    if (b != 2.0f) {
        return 2;
    }
    if (c != 3.0f) {
        return 3;
    }
    if (d != 4.0f) {
        return 4;
    }
    if (e != -1.0f) {
        return 5;
    }
    if (f != -2.0f) {
        return 6;
    }
    if (g != -3.0f) {
        return 7;
    }
    if (h != -4.0f) {
        return 8;
    }
    return 0;
}