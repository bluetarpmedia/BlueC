int main(void) {
    _Bool t = 1;
    _Bool f = 0;

    if ((t & t) != 1) return 1;
    if ((t & f) != 0) return 2;

    if ((t | f) != 1) return 3;
    if ((f | f) != 0) return 4;

    if ((t ^ t) != 0) return 5;
    if ((t ^ f) != 1) return 6;

    // _Bool promotes to int and when cast/assigned back to _Bool, any non-zero is 1.
    _Bool compl_t = ~t; 
    if (compl_t != 1) return 7; 
    
    _Bool compl_f = ~f;
    if (compl_f != 1) return 8;

    // 1 << 1 == 2 (int). When assigned back to _Bool, it becomes 1.
    _Bool shift_l = (t << 1);
    if (shift_l != 1) return 9;

    // 1 >> 1 = 0.
    _Bool shift_r = (t >> 1);
    if (shift_r != 0) return 10;

    // b &= 0 is the same as b = (int)b & 0
    _Bool b = 1;
    b &= 0;
    if (b != 0) return 11;

    return 0;
}