int get(void) {
    return 1;
}

int main(void)
{
    return (int (*fn)(void))get;
}