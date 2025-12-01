int main(void) {
    // Can't perform compound bitwise operations with floats
    float f = 1.0f;
    f &= 0;
    return (int) f;
}