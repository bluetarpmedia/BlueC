static int c = 0;

static int foo(void) {
    return 2;
}

typedef int Int32; // No warning for global unused typedef

int main(void) {
    typedef int MyInt;
    int f = 0;
    return 0;
}
