int main(void) {
    char x = 127;
    char y = 1;

    char a = x + y;
    char b = a + 1;
    char c = ~b;

    return c;
}
