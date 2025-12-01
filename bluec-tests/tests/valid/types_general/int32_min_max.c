int main(void) {
    int a = -2147483648;
    int b = -(2147483648);
    int c = (2147483647 * -1) - 1;
    int d = 2147483647;
    int e = (2147483647);
    return a == b && b == c && d == e;
}