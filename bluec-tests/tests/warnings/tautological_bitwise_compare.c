int main(void) {
    int a = 1, b = 0;
    b = a | 2;        // No warning
    b = 2 | a;        // No warning
    b = a & (a | 2);  // No warning

    // Boolean context but Bitwise OR with zero
    b = a && (a | 0); // No warning
    b = a && (0 | a); // No warning

    // Boolean context and Bitwise OR with non-zero
    //

    b = a && (a | 2);
    b = (2 | a) || a;

    b = !(a | 2);

    _Bool result = 0;
    result = (_Bool)(a | 2);  // No warning
    result = a | 2;

    if (a | 1) {
        return 1;
    }

    return 0;
}