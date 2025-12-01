/* Test addition, subtraction, multiplication, division, and negation with long doubles */

long double point_one = 0.1L;
long double point_two = 0.2L;
long double point_three = 0.3L;

long double two = 2.0L;
long double three = 3.0L;
long double four = 4.0L;
long double twelveE30 = 12e30L;

int addition(void) {
    return (point_one + point_two == 0.30000000000000004L);
}

int subtraction(void) {
    return (four - 1.0L == 3.0L);
}

int multiplication(void) {
    return (0.01L * point_three == 0.003L);
}

int division(void) {
    return (7.0L / two == 3.5L);
}

int negation(void) {
    double neg = -twelveE30;
    return !(12e30L + neg);
}

int complex_expression(void) {
    /* Test a more complex expression.
     * Note: all intermediate results in this expression
     * can be represented exactly, so we don't need to
     * consider the impact of rounding intermediate results.
     */

    double complex_expression = (two + three) - 127.5L * four;
    return complex_expression == -505.0L;
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