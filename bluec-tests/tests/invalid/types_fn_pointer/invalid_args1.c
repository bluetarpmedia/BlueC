int square(int x) {
    return x * x;
}

int main(void)
{
    int (*sq)(int) = square;

    sq();
    
    return 0;
}