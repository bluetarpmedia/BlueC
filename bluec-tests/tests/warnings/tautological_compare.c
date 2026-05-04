int main(void) {
    int a = 1;
    int res = 0;

    res = (a == a);
    res = (a != a);
    res = (a < a);
    res = (a > a);
    res = (a <= a);
    res = (a >= a);

    return res;
}