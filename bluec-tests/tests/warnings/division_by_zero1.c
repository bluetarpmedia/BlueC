int main(void) {
    int a = 1 / 0;
    int b = 100 / ((10 * 2) - 20);
    int c = -1 % 0;
    int d = -100 % (50 - 50);

    unsigned long e = (1 << 10) / (1 >> 1);
    unsigned int f = 11 / 0;
    unsigned short g = 12 / (12 - 12);
    unsigned char h = 'b' / ('a' - 'a');

    return a + b + c + d + (int)(e + f + g + h);
}