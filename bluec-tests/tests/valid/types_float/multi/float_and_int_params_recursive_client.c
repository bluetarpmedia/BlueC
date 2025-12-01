/* This test case is identical to chapter13/valid/function_calls/double_and_int_params_recursive.c
 * but split across two files */
int fun(int i1, float d1, int i2, float d2, int i3, float d3,
        int i4, float d4, int i5, float d5, int i6, float d6,
        int i7, float d7, int i8, float d8, int i9, float d9);
int main(void) {
    float f = fun(1, 2.0f, 3, 4.0f, 5, 6.0f, 7, 8.0f, 9, 10.0f, 11, 12.0f, 13, 14.0f, 15, 16.0f, 17, 18.0f);
    return (f == 78.00);
}