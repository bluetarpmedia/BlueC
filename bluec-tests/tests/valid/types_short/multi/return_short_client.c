short add(short a, short b) ;

int main(void) {
    short a = add(123, 400);
    /* Test returning a short from a function call */
    if (a != 523) {
        return 1;
    }
    return 0;
}