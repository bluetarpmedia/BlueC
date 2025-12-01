int main(void) {
    typedef int MyType;
    typedef signed int MyType;
    typedef signed MyType;

    MyType a = -1;

    {
        MyType b = 123;

        typedef float MyType;
        float typedef MyType;

        MyType c = 0.01f;

        {
            MyType d = 3.14f;

            typedef unsigned short MyType;
            typedef short unsigned MyType;

            MyType e = 65536;

            if (a != -1) {
                return 1;
            }

            if (b != 123) {
                return 2;
            }

            if (c != 0.01f) {
                return 3;
            }

            if (d != 3.14f) {
                return 4;
            }

            if (e != 0) {
                return 5;
            }
        }
    }

    return 0;
}
