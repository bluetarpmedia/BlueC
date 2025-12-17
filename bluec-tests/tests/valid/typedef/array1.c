int main(void) {
    typedef int MyIntArray[3];
    MyIntArray arr = {1, 2, 3};
    return arr[0] + arr[2];
}