float get(void) {
    return 1.0f;
}

int (*fn)(void) = get;

int main(void) {
    return 0;
}