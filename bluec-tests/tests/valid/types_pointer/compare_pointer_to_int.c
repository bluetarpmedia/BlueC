int main(void)
{
    long *l = 0;
    // Like clang/gcc, we allow this and emit a warning.
    return l <= 100ul;
}