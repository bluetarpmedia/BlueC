int main(void)
{
    // Like clang/gcc, we allow this but emit a warning.
    static int x = {1, 2, 3};
    return x;
}