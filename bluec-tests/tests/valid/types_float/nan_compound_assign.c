// Test that we handle NaN correctly for compound assignments
// All compound assignments to NaN are also NaN

int float_isnan(float f); // defined in dependencies/math.c

int main(void) {
    static float zero = 0.0f;
    float nan = 0.0f / zero; // make this constant-folding proof

    if (!float_isnan(nan += 99.2f)) {
        return 1;
    }

    if (!float_isnan(nan -= nan)) {
        return 2;
    }

    if (!float_isnan(nan *= 4.0f)) {
        return 3;
    }

    if (!float_isnan(nan /= 0.0f)) {
        return 4;
    }

    return 0;
}