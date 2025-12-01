int main(void) {
    int *a = 0;
    int *b = (10 > 1) ? a : 10;
    return b == 0;
}