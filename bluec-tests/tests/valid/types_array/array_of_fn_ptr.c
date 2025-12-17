int get1(void) {
    return 1;
}

int get2(void) {
    return 2;
}

int get3(void) {
    return 3;
}

int main(void)
{
    int (*fn_ptrs[3])(void);
    
    fn_ptrs[0] = get1;
    fn_ptrs[1] = get2;
    fn_ptrs[2] = get3;

    return fn_ptrs[1]() + fn_ptrs[2]();
}
