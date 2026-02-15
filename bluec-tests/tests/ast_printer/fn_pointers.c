int add(int a, int b) {
    return a + b;
}

int multiply(int a, int b) {
    return a * b;
}

int switch_to_multiply(int (**ptr_to_ptr)(int, int)) {
    *ptr_to_ptr = multiply;
    return 0;
}

int main(void) {
    int x = 10;
    int y = 5;

    int (*fn_ptr)(int, int) = add;

    if (fn_ptr(x, y) != 15) {
        return 1;
    }

    fn_ptr = &multiply;

    if ((*fn_ptr)(100, 2) != 200) {
        return 2;
    }

    fn_ptr = &add;

    if ((*fn_ptr)(x, y) != 15) {
        return 3;
    }

    int (**ptr_to_object)(int, int) = &fn_ptr;
    switch_to_multiply(ptr_to_object);

    if (fn_ptr(x, y) != 50) {
        return 4;
    }

    if ((**ptr_to_object)(x, y) != 50) {
        return 5;
    }

    fn_ptr = add;

    if ((*fn_ptr)(-50, -50) != -100) {
        return 6;
    }

    return 0;
}