int main(void) {
    /* It's illegal to apply bitwise | to floats */
    float f = 0.0f | -0.0;
    return 0;
}