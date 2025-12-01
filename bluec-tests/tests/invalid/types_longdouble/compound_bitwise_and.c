int main(void) {
    // Can't perform compound bitwise operations with floats
    long double f = 1.0L;
    f &= 0;
    return (int) f;
}