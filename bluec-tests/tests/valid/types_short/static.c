static short file_init = 7;
static short file_zero;

static short file_incrementer(void) {
    file_init += 1;
    return file_init;
}

static short call_block_static(void) {
    static short block_val = 3;
    short ret = block_val;
    block_val += 1;
    return ret;
}

int main(void) {
    /* Test 1: file-scope static has the initializer value */
    if (file_init != 7) return 1;

    /* Test 2: file-scope static without initializer is zero-initialized */
    if (file_zero != 0) return 2;

    /* Test 3: modifying file-scope static via a function persists */
    if (file_incrementer() != 8) return 3; /* file_init became 8 */
    if (file_incrementer() != 9) return 4; /* file_init became 9 */

    /* Test 4: block-scope static initializes once and persists across calls */
    if (call_block_static() != 3) return 5; /* first call returns initial 3 */
    if (call_block_static() != 4) return 6; /* second call returns 4 */

    /* Test 5: block-scope static retains value even if function called many times */
    if (call_block_static() != 5) return 7; /* third call returns 5 */

    /* Test 6: file-scope static and block-scope static are independent */
    /* file_init currently 9, increment it and ensure block static is unchanged in next call */
    if (file_incrementer() != 10) return 8; /* file_init -> 10 */
    /* next block call should return 6 (it was 5 previously) */
    if (call_block_static() != 6) return 9;

    /* Test 7: ensure repeated calls keep changing file_init as expected */
    if (file_incrementer() != 11) return 10;
    if (file_incrementer() != 12) return 11;

    return 0;
}
