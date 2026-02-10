int main(void) {
    int a = 0;

    // OK
    a = 0.0;
    a = 1.0f;
    a = -1.0f;
    a = 123.0;
    a = -456.0f;

    // UB
    a = -1.1f;
    a = -(1.0f + 0.1f);
    a += 1.1f;

    // Explicit cast is still UB
    a = (int)-1.1f;

    int b = 2.1;

    return a + b;
}