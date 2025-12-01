// Can't apply %= to a float
int main(void) {
    int i = 5;
    i %= 1.0L;
    return i;
}