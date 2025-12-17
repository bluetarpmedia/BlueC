int main(void)
{
    long arr[5][4] = { {0} };

    for (int i = 0; i < 5; ++i)
        for (int j = 0; j < 4; ++j)
            if (arr[i][j] != 0)
                return 1;

    long counter = 1;
    for (int i = 0; i < 5; ++i)
        for (int j = 0; j < 4; ++j)
            arr[i][j] = counter++;

    long check = 1;
    for (int i = 0; i < 5; ++i)
        for (int j = 0; j < 4; ++j)
            if (arr[i][j] != check++)
                return 2;

    if (arr[3][3] != 16)
        return 3;

    return 0;
}