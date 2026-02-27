int main(void) {
    char text[5] = "test";
    _Bool b1 = text;

    int arr[3] = {};
    _Bool b2 = arr;

    return b1 + b2;
}