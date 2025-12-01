/* Test compound assignment with floats */
int main(void) {
    float f = 10.0f;
    f /= 4.0f;
    if (f != 2.5f) {
        return 1;
    }
    f *= 10000.0f;
    if (f != 25000.0f) {
        return 2;
    }
    return 0;
}