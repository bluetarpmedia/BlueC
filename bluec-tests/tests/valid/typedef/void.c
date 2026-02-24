typedef void VoidType;
typedef VoidType* VoidPtr;

VoidType modify_n(int *n) {
    *n = 100;
}

VoidPtr ident_ptr(VoidPtr ptr) {
    return ptr;
}

int main(void) {
    int val = 0;
    int target = 42;

    VoidPtr p = &target;
    VoidPtr q = ident_ptr(p);

    if (*(int *)q != 42) {
        return 1;
    }

    modify_n(&val);
    if (val != 100) {
        return 2;
    }

    return 0; // Success
}