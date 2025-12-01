long long add(int a, int b);

int main(void) {
    long long a = add(2147483645, 2147483645);
    /* Test returning a long from a function call */
    if (a != 4294967290LL) {
        return 1;
    }
    return 0; // success
}