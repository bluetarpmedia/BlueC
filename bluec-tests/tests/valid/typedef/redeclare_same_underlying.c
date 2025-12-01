int main(void) {
    typedef int foo;
    typedef int MyInt;
    typedef MyInt foo;
    foo x = 3;
    return x;
}
