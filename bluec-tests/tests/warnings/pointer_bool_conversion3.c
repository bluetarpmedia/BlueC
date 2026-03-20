int add(int a, int b) {
    return a + b;
}

int main(void) {
    _Bool exists1 = add;
    _Bool exists2 = &add;        // No warning
    _Bool exists3 = (_Bool)add;  // No warning
    _Bool exists4 = (_Bool)&add; // No warning
    return exists1 + exists2 + exists3 + exists4;
}