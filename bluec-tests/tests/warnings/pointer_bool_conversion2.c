int main(void) {
    char text[5] = "test";
    _Bool b1 = text;

    int arr[3] = {};
    _Bool b2 = arr;

    _Bool b3 = (_Bool)text; // No warning
    _Bool b4 = (_Bool)arr;  // No warning

    return b1 + b2 + b3 + b4;
}