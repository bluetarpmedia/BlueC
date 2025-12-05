int square(int n) {
    return n * n;
}

int cube(int n) {
    return n * n * n;
}

static int SQUARE_MODE = 0;
static int CUBE_MODE = 1;

typedef int (*OperationFuncPtr)(int);

OperationFuncPtr get_operation(int mode) {
    if (mode == SQUARE_MODE) {
        return square;
    } else {
        return &cube;
    }
}

int main(void) {
    OperationFuncPtr op1 = get_operation(SQUARE_MODE);

    if (op1(3) != 9) {
        return 1; 
    }

    OperationFuncPtr op2 = get_operation(CUBE_MODE);

    if (op2(3) != 27) {
        return 2; 
    }
    
    return 0;
}