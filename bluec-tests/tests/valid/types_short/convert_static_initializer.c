short i = 8589934592L; // 2^33, truncated to 0

int main(void) {
    if (i != 0) {
        return 1;
    }
    return 0;
}