int main(void) {
    if (_Alignof(int) != 4) {
        return 1;
    }

    if (_Alignof(double) != 8) {
        return 2;
    }

    return 0;
}