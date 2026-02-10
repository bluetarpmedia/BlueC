int main(void) {
    double d = 1.0;
    float  f = d;
    f = d;
    f += d;
    return f == 1.0f;
}