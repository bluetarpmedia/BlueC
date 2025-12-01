int main(void) {
    typedef int MyType;
    typedef signed int MyType;
    typedef signed MyType;

    {
        typedef float MyType;
        float typedef MyType;

        typedef double MyType;  // Error
    }

    return 0;
}
