int main(void) {
    _Bool t = 1;
    _Bool f = 0;

    t++;
    --f;
    _Bool complement = ~t; 

    return complement;
}