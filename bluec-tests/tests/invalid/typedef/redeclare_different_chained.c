int main(void) {
    typedef int foo;
    typedef foo bar;
    typedef bar baz;

    typedef float AA;
    typedef AA BB;
    typedef BB baz;

    baz x = 5;
    return x;
}
