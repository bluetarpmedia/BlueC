int main(void) {
    short a = -32768;
    short b = -(32768);
    short c = (32767 * -1) - 1;
    short d = 32767;
    short e = (32767);
    return a == b && b == c && d == e;
}