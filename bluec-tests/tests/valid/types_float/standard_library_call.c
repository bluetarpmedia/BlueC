/* Make sure we can call floating-point functions from the standard library */

/* We need to declare these functions ourselves since we can't #include <math.h> */

// fused multiply and add: (x * y) + z
// note: only the final result of the whole calculation is rounded,
// not the intermediate result x * y
float fmaf(float x, float y, float z);

float ldexpf(float x, int exp); // x * 2^exp

int main(void) {
    float fma_result = fmaf(5.0f, 0.1f, 3.0f);
    float ldexp_result = ldexpf(0.5f, 5);
    if (fma_result != 3.5f) {
        return 1;
    }

    if (ldexp_result != 16.0f) {
        return 2;
    }

    return 0;
}