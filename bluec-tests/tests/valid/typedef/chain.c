int main(void) {
    typedef int foo;
    typedef foo bar;
    typedef bar baz;
    baz x = 11;
    return x;
}
