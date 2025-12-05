int main(void)
{
    double (*fn1)(void) = 0;
    long (*fn2)(void) = 0;

    fn1 = fn2;
    
    return 0;
}