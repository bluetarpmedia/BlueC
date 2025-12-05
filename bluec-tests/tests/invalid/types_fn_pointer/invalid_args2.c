int square(int x) {
    return x * x;
}

int main(void)
{
    int (*sq)(int) = square;

    sq(5, 6);
    
    return 0;
}