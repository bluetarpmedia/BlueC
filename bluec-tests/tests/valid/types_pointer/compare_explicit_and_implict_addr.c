int main(void)
{
    int arr[10];
    // Like clang/gcc, we allow this and emit a warning.
    return arr == &arr;
}