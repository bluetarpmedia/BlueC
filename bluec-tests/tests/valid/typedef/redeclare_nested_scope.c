int main(void) {
    typedef int MyInt;

    MyInt x = 3;
    if (x > 2) {
        typedef int AnotherInt;
        typedef AnotherInt MyInt;
        MyInt x = 5;

        if (x >= 5) {
            typedef int Int32;
            typedef Int32 my_int32_t; 
            typedef my_int32_t MyInt;
            MyInt x = 0;
            return x == 0;
        }
    }

    MyInt i = 10;
    return i;
}