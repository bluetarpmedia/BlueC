_Bool b = 1;

int main(void)
{
    // this conflicts with previous definition of 'b'
    extern char b;
    return b;
}