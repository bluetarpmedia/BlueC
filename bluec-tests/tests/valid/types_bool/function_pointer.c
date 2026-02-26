_Bool identity(_Bool b) {
    return b;
}

_Bool is_nonzero(int i) {
    return i; 
}

_Bool negate(_Bool b) {
    return !b;
}

void flip_in_place(_Bool *b) {
    *b = !(*b);
}

int main(void) {
    _Bool (*func_ptr)(_Bool) = identity;
    if (func_ptr(1) != 1) return 1;
    if (func_ptr(0) != 0) return 2;

    _Bool (*check_ptr)(int) = is_nonzero;
    if (check_ptr(1234) != 1) return 3;
    if (check_ptr(0) != 0) return 4;

    if (identity(99) != 1) return 5;

    _Bool ptr_exists = (_Bool)identity;
    if (ptr_exists != 1) return 6;

    _Bool (*null_func)(_Bool) = 0;
    _Bool null_exists = (_Bool)null_func;
    if (null_exists != 0) return 7;

    _Bool (*logic_gates[2])(_Bool) = { identity, negate };
    if (logic_gates[0](1) != 1) return 8;
    if (logic_gates[1](1) != 0) return 9;

    _Bool val = 0;
    flip_in_place(&val);
    if (val != 1) return 10;

    if (identity(identity(identity(1))) != 1) return 11;

    return 0;
}