int global[3 * 3] = {1, 2, 3, 4, 5, 6, 7, 8, 9};

int main(void) {
    int arr[3 * 2 + 1] = {1, 2, 3, 4, 5, 6, 7};
    int sum = 0;
    for (int i = 0; i < 7; ++i) {
        sum += arr[i];
    }
    return sum + global[8];
}