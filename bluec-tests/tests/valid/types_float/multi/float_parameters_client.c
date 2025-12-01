/* This test case is identical to chapter13/valid/function_calls/double_parameters.c
 * but split across two files */
int check_arguments(float a, float b, float c, float d, float e, float f, float g, float h);

int main(void) {
    return check_arguments(1.0f, 2.0f, 3.0f, 4.0f, -1.0f, -2.0f, -3.0f, -4.0f);
}