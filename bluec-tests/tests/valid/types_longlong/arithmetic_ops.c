/* Test basic arithmetic operations on long long integers
 * when one or both operands and the result are outside the range of int */

long long a;
long long b;

int addition(void) {
    // a == 4294967290l, i.e. 2^32 - 6
    // b = 5
    return (a + b == 4294967295LL);
}

int subtraction(void) {
    // a = -4294967290l;
    // b = 90l;
    return (a - b == -4294967380LL);
}

int multiplication(void) {
    // a = 4294967290l;
    return (a * 4l == 17179869160LL);
}

int division(void) {
    /* The first operand can't fit in an int; this requires us to store the operand in RDX:RAX
    * using the 'cqo' instruction, instead of in EDX:EAX using 'cdq'
    */
    // a = 4294967290l;
    b = a / 128l;
    return (b == 33554431LL);
}

int remaind(void) {
    // a = 8589934585l, i.e. 2^33 - 7
    b = -a % 4294967290LL;
    return (b == -5l);
}

int complement(void) {
    // a = 9223372036854775806l, i.e. LONG_MAX - 1
    return (~a == -9223372036854775807LL);
}

int main(void) {

    /* Addition */
    a = 4294967290LL; // 2^32 - 6
    b = 5l;
    if (!addition()) {
        return 1;
    }

    /* Subtraction */
    a = -4294967290LL;
    b = 90l;
    if (!subtraction()) {
        return 2;
    }

    /* Multiplication */
    a = 4294967290LL;
    if (!multiplication()) {
        return 3;
    }

    /* Division */
    a = 4294967290LL;
    if (!division()) {
        return 4;
    }

    /* Remainder */
    a = 8589934585LL; // 2^33 - 7
    if (!remaind()) {
        return 5;
    }

    /* Complement */
    a = 9223372036854775806LL; //LONG_MAX - 1
    if (!complement()) {
        return 6;
    }

    return 0;
}