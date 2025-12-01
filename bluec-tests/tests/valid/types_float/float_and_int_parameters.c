/* Test that we've properly implemented the calling convention
 * for passing floats and ints in registers
 */
int check_arguments(float f1, float f2, int i1, float f3, float f4, int i2, int i3,
                    int i4, float f5, float f6, float f7, int i5, float f8) {

    if (f1 != 1.0f) {
        return 1;
    }
    if (f2 != 2.0f) {
        return 2;
    }
    if (f3 != 3.0f) {
        return 3;
    }
    if (f4 != 4.0f){
        return 4;
    }
    if (f5 != 5.0f){
        return 5;
    }
    if (f6 != 6.0f){
        return 6;
    }
    if (f7 != 7.0f){
        return 7;
    }
    if (f8 != 8.0f){
        return 8;
    }
    if (i1 != 101){
        return 9;
    }
    if (i2 != 102){
        return 10;
    }
    if (i3 != 103){
        return 11;
    }
    if (i4 != 104) {
        return 12;
    }
    if (i5 != 105) {
        return 13;
    }
    return 0;
}

int main(void) {
    return check_arguments(1.0f, 2.0f, 101, 3.0f, 4.0f, 102, 103, 104, 5.0f, 6.0f, 7.0f, 105, 8.0f);
}