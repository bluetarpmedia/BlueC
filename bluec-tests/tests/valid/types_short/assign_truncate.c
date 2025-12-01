short g_a = 32767;
short g_b = 32768;
short g_c = -32768;

int main(void) {
    short a = 32767;
    short b = 32768;
    short c = -32768;

    return a == 32767 && b == -32768 && b == c && a == g_a && b == g_b && c == g_c;
}
