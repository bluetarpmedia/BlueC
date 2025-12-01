int main(void) {

    /* bitwise AND promotion */
    short and_l = 3855;
    short and_r = 255;
    if ((and_l & and_r) != 15) return 1;

    /* bitwise OR promotion */
    short or_l = 3840;
    short or_r = 240;
    if ((or_l | or_r) != 4080) return 2;

    /* bitwise XOR promotion */
    short xor_l = -1;
    short xor_r = 4095;
    if ((xor_l ^ xor_r) != -4096) return 3;

    /* bitwise NOT promotion */
    short t0 = 0;
    if (~t0 != -1) return 4;

    /* left shift promotion */
    short shl = 1;
    if ((shl << 8) != (1 << 8)) return 5;

    /* right shift promotion on a positive value */
    short shr_pos = 256;
    if ((shr_pos >> 8) != 1) return 6;

    /* combined bitwise and arithmetic promotions */
    short c1 = 255;
    short c2 = 3855;
    if ((~c1 & c2) != 3840) return 7;

    return 0;
}
