_Bool return_true(void);
_Bool return_false(void);

int main(void) {
    _Bool bool_array1[3] = {1, 0, 1};
    _Bool t = return_true();
    _Bool bool_array2[3] = {0, 1, 0};
    _Bool f = return_false();
    _Bool bool_array3[3] = {1, 1, 1};

    // make sure we got the right return values and didn't overwrite
    // other arrays on the stack
    if (bool_array1[0] != 1 || bool_array1[1] != 0 || bool_array1[2] != 1) {
        return 1;
    }

    if (t != 1) {
        return 2;
    }

    if (bool_array2[0] != 0 || bool_array2[1] != 1 || bool_array2[2] != 0) {
        return 3;
    }

    if (f != 0) {
        return 4;
    }

    if (bool_array3[0] != 1 || bool_array3[1] != 1 || bool_array3[2] != 1) {
        return 5;
    }
    
    return 0;
}