/* The result of a cast expression is not an lvalue */

int main(void) {
    int i = 0;
    i = (short) i = 10;
    return 0;
}