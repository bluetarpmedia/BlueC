int main(void) {
    long lng = 1LL;

    float f = lng;
    f = lng;
    f += lng;

    double d = lng;
    d = lng;
    d += lng;

    return f == 1.0f && d == 1.0;
}