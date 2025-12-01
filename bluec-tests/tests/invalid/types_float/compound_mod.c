// Can't apply %= to a float
int main(void) {
    float f = 5.0f;
    f %= 2;
    return (int) f;
}