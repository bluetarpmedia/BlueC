int main(void) {
    // _Alignof result is a size_t/ulong, so _its_ size is 8
    if (sizeof _Alignof(char) != 8) {
        return 1;
    }

    // make sure _Alignof result is unsigned
    // since the common type of ulong and int is ulong,
    // the result of subtraction here will be positive unsigned int
    // (and 0 in comparison will also be converted to 0u)
    if (_Alignof(float) - _Alignof(float) - 1 < 0) {
        return 2;
    }

    return 0;
}