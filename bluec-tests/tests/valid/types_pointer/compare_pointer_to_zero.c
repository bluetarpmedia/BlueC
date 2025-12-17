int main(void)
{
    int *x = 0;
    // Like clang/gcc, we allow this and emit a warning.
    return x > 0;
}