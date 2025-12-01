float fabsf(float arg);

int non_zero(float f) {
    return !f;
}

float multiply_by_large_num(float f) {
    return f * 1e40;
}

int main(void) {

    /* Make sure subnormal numbers are not rounded to zero */
    float subnormal = 1.0e-40f;

    /* Perform an operation on a subnormal number to produce a normal number */
    float f = multiply_by_large_num(subnormal);
    if (fabsf(1.0f - f) > 0.001f) {
        return 2;
    }

    // subnormal is non-zero, so !subnormal should be zero
    return non_zero(subnormal);
}