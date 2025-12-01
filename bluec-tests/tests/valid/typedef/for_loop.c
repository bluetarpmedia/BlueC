int main(void) {
    int i = 0, j = 0;

    for (typedef int MyInt; i < 5; i++) {
        MyInt x = i;
        j += x;
    }

    return j;
}
