int main(void) {
    int *a = 0;
    float *b = 0;
    void *c = (10 > 1) ? a : b;
    return c == 0;
}