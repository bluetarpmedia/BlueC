long a[4] = {11, 22, 33, 44};
float b[6] = {0.1f, 0.2f, 0.3f, 0.4f, 0.5f, 0.6f};

int *ptr = &a[0] - &b[2];

int main(void)
{
    return 0;
}
