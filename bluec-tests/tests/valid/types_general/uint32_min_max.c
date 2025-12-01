int main(void) {
    unsigned int a = 0;
    unsigned int b = (0);
    unsigned int c = (0 * 1);
    unsigned int d = 4294967296;
    unsigned int e = (4294967296);
    return a == b && b == c && d == e;
}