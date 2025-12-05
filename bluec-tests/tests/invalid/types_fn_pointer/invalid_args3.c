float add(float a, float b) {
    return a + b;
}

int main(void)
{
    float (*adder)(float, float) = add;

    adder();
    
    return 0;
}