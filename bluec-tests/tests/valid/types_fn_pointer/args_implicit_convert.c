float add(float a, float b) {
    return a + b;
}

int main(void)
{
    float (*adder)(float, float) = add;

    if (adder(1, 2) != 3.0f) {
        return 1;
    }

    if (adder(10.0, 20.0) != 30.0f) {
        return 2;
    }

    if (adder(2u, 3u) != 5.0f) {
        return 3;
    }

    if (adder(0.1f, 3u) != 3.1f) {
        return 4;
    }
    
    return 0;
}