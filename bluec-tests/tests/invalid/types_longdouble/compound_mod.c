// Can't apply %= to a float
int main(void) {
    long double f = 5.0l;
    f %= 2;
    return (int) f;
}