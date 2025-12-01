int main(void) {
    // Can't perform compound bitwise operations with floats
    int i = 1000;
    i >>= 2.0L;
    return i;
}