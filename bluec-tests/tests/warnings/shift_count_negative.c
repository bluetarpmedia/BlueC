int main(void) {
    int a = 1 >> -1;
    
    a = 1 << (9 - 12);

    int b = 1;
    a = b << -5 * 1;

    return a;
}