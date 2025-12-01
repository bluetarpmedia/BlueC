// Test that we handle NaN correctly for increments and decrements
// All increments and decrements on NaN are also NaN

int float_isnan(float f); // defined in dependencies/math.c

int main(void) {
    static float zero = 0.0f;
    float nan = 0.0f / zero; // make this constant-folding proof

    if (!float_isnan(++nan)) {
        return 1;
    }

    if (!float_isnan(--nan)) {
        return 2;
    }

    if (!float_isnan(nan++)) {
        return 3;
    }

    if (!float_isnan(nan--)) {
        return 4;
    }

    return 0;
}