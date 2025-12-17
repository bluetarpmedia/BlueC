// Empty initializer lists are allowed in C23, and also allowed by clang/gcc prior as an extension
int main(void) {
    int arr[1] = {};
    return arr[0];
}