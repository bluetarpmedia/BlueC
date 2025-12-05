int main(void) {
    short (*fn)(short) = 0;
    int i = (int)fn;

    int *ptr = 0;
    int j = (short)ptr;

    return i == j;
}