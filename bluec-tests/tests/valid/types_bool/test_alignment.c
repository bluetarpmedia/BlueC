// Test that any _Bool array variables larger than 16 bytes are 16-byte aligned

int check_aligment(_Bool *b) {
    unsigned long l = (unsigned long)b;
    return (l % 16 == 0);  // return 1 on success, 0 on failure
}

// define some static arrays that are >= 16 bytes
static _Bool flat_static[16] = {1};
static _Bool nested_static[3][4][2] = {{{1}, {0}}};

int main(void) {
    // define some automatic arrays that are >= 16 bytes
    _Bool flat_auto[22];
    _Bool nested_auto[10][3];

    if (!check_aligment((_Bool *)flat_static)) {
        return 1;
    }

    if (!check_aligment((_Bool *)nested_static)) {
        return 2;
    }

    if (!check_aligment((_Bool *)flat_auto)) {
        return 3;
    }

    if (!check_aligment((_Bool *)nested_auto)) {
        return 4;
    }

    return 0;
}