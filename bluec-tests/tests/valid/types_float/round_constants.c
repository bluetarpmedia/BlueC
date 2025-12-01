int float_isinf(float f); // defined in dependencies/math.c

int main(void) {
    /* Test 1: Value too small to represent rounds to nearest float */
    if (1.00000000001f != 1.0f) {
        return 1;
    }

    /* Test 2: Value between two floats rounds to nearest even */
    if (16777217.0f != 16777216.0f) {
        return 2;
    }

    /* Test 3: Another small increment that rounds down */
    if (1.0000001f == 1.0f) {
        return 3;
    }

    /* Test 4: Value exceeds max float, rounds to infinity */
    if (!float_isinf(3.5e38f)) {
        return 4;
    }

    /* Test 5: Negative value exceeds min float, rounds to -infinity */
    if (!float_isinf(-3.5e38f)) {
        return 5;
    }

    /* Test 6: Very small positive value rounds to zero */
    if (1e-50f != 0.0f) {
        return 6;
    }

    /* Test 7: Value just above smallest subnormal is preserved */
    float smallest = 1.4e-45f; /* Smallest positive subnormal float */
    if (smallest == 0.0f) {
        return 7;
    }

    /* Test 8: Test rounding with exact halfway case (round to even) */
    /* 16777217 is exactly halfway between 16777216 and 16777218 in float precision */
    /* Should round to 16777216 (even mantissa) */
    if (16777217.0f != 16777216.0f) {
        return 8;
    }

    /* Test 9: Large number precision loss */
    if (16777216.0f + 1.0f != 16777216.0f) {
        return 9;
    }

    /* Test 10: Verify maximum finite float doesn't overflow */
    if (float_isinf(3.4028234e38f)) {
        return 10;
    }

    return 0; /* success */
}