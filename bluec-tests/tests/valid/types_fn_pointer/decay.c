/* This program tests that expressions of function type decay to function pointers. */

int mul_by_2(int a) {
    return a * 2;
}

int test_decay_argument(int (*ptr)(int), int val) {
    return ptr(val);
}

int test_decay_param(int ptr(int), int val) {
    return ptr(val);
}

int main(void) {
    int (*fn)(int) = 0;

    fn = mul_by_2;
    
    if (fn(3) != 6) {
        return 1; 
    }

    if (test_decay_argument(mul_by_2, 4) != 8) {
        return 2; 
    }

    if (test_decay_param(mul_by_2, 7) != 14) {
        return 3; 
    }

    if (fn != mul_by_2) {
        return 4;
    }

    if (mul_by_2 != fn) {
        return 5;
    }

    return 0;
}