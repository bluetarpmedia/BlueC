int main(void) {
    /* It's illegal to apply bitwise & to floats */
    float f = 10.0f & -1;
    return 0;
}