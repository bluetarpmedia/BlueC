int main(void) {
    int x = 0;
    for (int i = 1, j = i + 5; i < 10; ++i) {
        j++;
        x = j;
    }
    return x;
}
