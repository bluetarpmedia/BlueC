#define BC_SHRT_MIN (-32768)

int main(void) {
    /* Test 1: simple matching of positive short value */
    short v1 = 123;
    switch (v1) {
    case 123: break;
    default: return 1;
    }

    /* Test 2: matching negative short value */
    short v2 = -45;
    switch (v2) {
    case -45: break;
    default: return 2;
    }

    /* Test 3: fall-through between cases */
    short v3 = 2;
    int reached = 0;
    switch (v3) {
    case 1: reached = 1; break;
    case 2: reached = 2;
    case 3: reached += 10; break;
    default: return 3;
    }
    /* case 2 falls through to case 3, so reached should be 12 */
    if (reached != 12) return 4;

    /* Test 4: default taken when no case matches */
    short v4 = 999;
    int d4 = 0;
    switch (v4) {
    case 1: d4 = 1; break;
    case 2: d4 = 2; break;
    default: d4 = 42; break;
    }
    if (d4 != 42) return 5;

    /* Test 5: switch expression promotions: a short compares equal to an int case label */
    short v5 = -1;
    switch (v5) {
    case -1: break;
    default: return 6;
    }

    /* Test 6: multiple case labels mapping to the same body */
    short v6 = 7;
    int out6 = 0;
    switch (v6) {
    case 5:
    case 6: out6 = 1; break;
    case 7:
    case 8: out6 = 2; break;
    default: return 7;
    }
    if (out6 != 2) return 8;

    /* Test 7: complex expression as switch selector (promotions should apply before comparison) */
    short a7 = 10;
    short b7 = 3;
    int sel7 = a7 / b7; /* 10/3 == 3 */
    switch (sel7) {
    case 3: break;
    default: return 9;
    }

    /* Test 8: large negative value within short range matches case label */
    short v8 = BC_SHRT_MIN + 1;
    switch (v8) {
    case (BC_SHRT_MIN + 1): break;
    default: return 10;
    }

    /* Test 9: verify that case labels are compared as integers, not by bit-pattern of a different width */
    short v9 = 256;
    switch (v9) {
    case 256: break;
    default: return 11;
    }

    /* Test 10: ensure switch with consecutive case labels executes only matching block */
    short v10 = 4;
    int out10 = 0;
    switch (v10) {
    case 1: out10 = 1; break;
    case 2: out10 = 2; break;
    case 3: out10 = 3; break;
    case 4: out10 = 4; break;
    default: out10 = -1; break;
    }
    if (out10 != 4) return 12;

    return 0;
}
