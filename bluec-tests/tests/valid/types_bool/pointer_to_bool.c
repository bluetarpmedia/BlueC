int main(void) {
    _Bool b = 0;
    _Bool *ptr = &b;

    *ptr = 1;
    if (b != 1) return 1;

    *ptr = 100;
    if (b != 1) return 2;

    *ptr = 1;
    (*ptr)++;
    if (b != 1) return 3;

    _Bool flags[2] = {0, 1};
    _Bool *arr_ptr = flags;
    if (*arr_ptr != 0) return 4;
    if (*(arr_ptr + 1) != 1) return 5;

    // Verify that the distance between two _Bools in an array is exactly sizeof(_Bool).
    if ((char *)&flags[1] - (char *)&flags[0] != sizeof(_Bool)) return 6;

    _Bool **pptr = &ptr;
    **pptr = 0;
    if (b != 0) return 7;

    _Bool *null_ptr = 0;
    if (null_ptr != (void*)0) return 8;

    _Bool pointer_is_valid = (_Bool)ptr;
    if (pointer_is_valid != 1) return 9;

    *ptr = 1;
    (*ptr)--;
    if (b != 0) return 10;

    *ptr = 1;
    --(*ptr);
    if (b != 0) return 11;

    *ptr = 0;
    (*ptr)--;
    if (b != 1) return 12;

    *ptr = 0;
    --(*ptr);
    if (b != 1) return 13;

    *ptr = 0;
    (*ptr)++;
    if (b != 1) return 14;

    *ptr = 0;
    ++(*ptr);
    if (b != 1) return 15;

    *ptr = 10;
    (*ptr)++;
    if (b != 1) return 16;

    *ptr = 10;
    ++(*ptr);
    if (b != 1) return 17;

    return 0;
}