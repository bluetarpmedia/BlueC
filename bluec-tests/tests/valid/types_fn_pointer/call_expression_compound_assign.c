int square(int x) {
    return x * x;
}

int main(void) {
    int (*fn)(int) = square;

    if ( (fn += 0)(3) != 9 ) {
        return 1;
    }

    if ( (fn -= 0)(4) != 16 ) {
        return 2;
    }

    if ( ((fn += 0))(5) != 25 ) {
        return 3;
    }

    return 0;
}