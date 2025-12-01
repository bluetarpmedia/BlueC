int main(void) {
    /* You can't apply the modulo operator to a float */
    float f = 10.0f;
    f = f % 3;
    return 0;
}