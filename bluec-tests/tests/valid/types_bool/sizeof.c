_Bool return_false(void) {
    return 0;
}

int main(void) {
    if (sizeof(_Bool) != 1) {
        return 1;
    }

    _Bool b = 1;
    if (sizeof b != 1) {
        return 2;
    }

    if (sizeof return_false() != 1) {
        return 3;
    }

    return 0;
}