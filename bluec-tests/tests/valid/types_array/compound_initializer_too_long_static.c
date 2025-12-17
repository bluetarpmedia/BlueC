int main(void) {
    // We permit a compound initializer that is too long and emit a warning.
    static int arr[3] = {1, 2, 3, 4};
    return arr[2];
}