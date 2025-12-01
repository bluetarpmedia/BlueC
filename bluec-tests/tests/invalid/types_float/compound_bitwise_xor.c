int main(void) {
    // Can't perform compound bitwise operations with floats
    int i = 0;
    i |= 2.0f;
    return (int) i;
}