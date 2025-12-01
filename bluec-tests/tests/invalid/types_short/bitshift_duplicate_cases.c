// Test that we correctly infer type of bitshift expression;
// x << y has type 'int', not 'short'.

int main(void) {
    short x = 100;
    switch (x << 2) {  // x << 2 == 400
        // these cases are duplicates b/c they'll both be converted to the type
        // of the switch expression - which is int, because of integer promotion.
        case 34359738768:  // 2**35 + 400
            return 1;
        case 400:
            return 0;
    }
    return 10;
}