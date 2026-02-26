_Bool b = 1;

int main(void)
{
    // this conflicts with previous definition of 'b'
    extern unsigned char b;
    return b;
}