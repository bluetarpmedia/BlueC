int sub_to_int(unsigned long a, unsigned long b) {
    return (int)(a - b);
}

int main(void) {
    int (*my_sub)(unsigned long, unsigned long) = sub_to_int;

    int a = my_sub(10, 0);
    int b = (*my_sub)(10, 10);
    int c = (*******my_sub)(2, 1);
    
    if (a != 10) {
        return 1;
    }

    if (b != 0) {
        return 2;
    }

    if (c != 1) {
        return 3;
    }

    return 0;
}