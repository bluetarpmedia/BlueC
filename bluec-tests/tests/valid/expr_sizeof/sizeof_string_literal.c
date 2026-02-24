int main(void) {
    if (sizeof "" != 1) {
        return 1;
    }

    if (sizeof "test" != 5) {
        return 2;
    }

    if (sizeof "this literal has spaces" != 24) {
        return 3;
    }

    return 0;
}