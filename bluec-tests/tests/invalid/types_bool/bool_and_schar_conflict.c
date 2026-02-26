_Bool b = 1;

int main(void)
{
    // this conflicts with previous definition of 'b'
    extern signed char b;
    return b;
}