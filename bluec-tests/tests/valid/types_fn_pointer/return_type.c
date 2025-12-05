/* This program verifies that a function can correctly return a function pointer. */

int square(int n) {
    return n * n;
}

int cube(int n) {
    return n * n * n;
}

static int SQUARE_MODE = 0;
static int CUBE_MODE = 1;

int (*get_operation(int mode))(int) {
    if (mode == SQUARE_MODE) {
        return square;
    } else {
        return cube;
    }
}

int main(void) {
    int value = 4;
    
    int (*op_ptr)(int);
    
    op_ptr = get_operation(SQUARE_MODE);

    if (op_ptr(value) != 16) {
        return 1; 
    }

    op_ptr = get_operation(CUBE_MODE);

    if (op_ptr(value) != 64) {
        return 2; 
    }
    
    return 0;
}