int main(void) {
    int a = 0;
    // Use a floating-point number as the controlling expression in a for loop
    // Normally this is a bad idea - rounding error might mean that the value will never
    // be exactly zero, so the loop won't terminate.
    // In this case we won't encounter rounding error, since we can exactly represent
    // every integer between 0 and 100 as a float.

    // First test with double literals
    for(float f = 50.0; f > 0.0; f = f - 1.0) {
        a = a + 1;
    }

    // Second test with float literals
    for(float f = 50.0f; f > 0.0f; f = f - 1.0f) {
        a = a + 1;
    }

    // Third test with long double literals
    for(float f = 50.0L; f > 0.0L; f = f - 1.0L) {
        a = a + 1;
    }

    return a;
}