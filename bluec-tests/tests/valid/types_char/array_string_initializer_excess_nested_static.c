char array[3][3] = {"a", "bcde"};

int main(void)
{
    return array[1][2] == 'd' && array[2][0] == 0;
}