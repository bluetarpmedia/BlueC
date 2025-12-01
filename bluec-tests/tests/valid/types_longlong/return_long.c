long long add(int a, int b) {
    return (long long) a + (long long) b;
}

int main(void) {
    long long a = add(2147483645, 2147483645);
    /* Test returning a long from a function call */
    if (a == 4294967290l) {
        return 1;
    }
    return 0;
}