/* Test addition, subtraction, multiplication, division, and negation with floats */

float point_one = 0.1f;
float point_two = 0.2f;
float point_three = 0.3f;

float two = 2.0f;
float three = 3.0f;
float four = 4.0f;
float twelveE30 = 12e30f;

int addition(void) {
    return (point_one + point_two == 0.30000000000000004f);
}

int subtraction(void) {
    return (four - 1.0f == 3.0f);
}

int multiplication(void) {
    return (0.01f * point_three == 0.003f);
}

int division(void) {
    return (7.0f / two == 3.5f);
}

int negation(void) {
    float neg = -twelveE30;
    return !(12e30f + neg);
}

int complex_expression(void) {
    /* Test a more complex expression.
     * Note: all intermediate results in this expression
     * can be represented exactly, so we don't need to
     * consider the impact of rounding intermediate results.
     */

    float complex_expression = (two + three) - 127.5f * four;
    return complex_expression == -505.0f;
}

int main(void) {

    if (!addition()) {
        return 1;
    }

    if (!subtraction()){
        return 2;
    }

    if (!multiplication()) {
        return 3;
    }

    if (!division()) {
        return 4;
    }

    if (!negation()) {
        return 5;
    }

    if (!complex_expression()) {
        return 5;
    }

    return 0;
}