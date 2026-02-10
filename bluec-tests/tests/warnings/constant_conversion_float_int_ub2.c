int main(void) {
    unsigned long a = 0;

    // OK
    a = 0.0f;
    a = 1.0f;
    a = 55.0;
    a = 123.0;
    
    // UB
    a = -1.0f;
    a = -1.1f;
    a = (1.0f + 0.1f);
    a += 1.1f;

    // Explicit cast is still UB
    a = (unsigned long)1.1f;

    unsigned char b = 2.1;

    return (int)a + (int)b;
}