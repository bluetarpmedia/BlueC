int main(void) {
    typedef int* MyIntPtr;
    typedef float* MyFloatPtr;

    int i = 5;
    MyIntPtr pi = &i;

    float f = 1.0f;
    MyFloatPtr pf = &f;

    float value = *pi + *pf;
    if (value != 6.0f) {
        return 1;
    }

    return 0;
}