/* Can't declare the same function with two return types: unsignd int and unsigned short */
unsigned int foo(void);

unsigned short foo(void) {
    return 0;
}

int main(void) {
    return 0;
}