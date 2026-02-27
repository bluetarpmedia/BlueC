int calc(void) { return 1; }
int x = 1;

int main(void) {
    static _Bool b1 = calc;
    static _Bool b2 = &calc;
    static _Bool b3 = &x;
    static _Bool b4 = "test";

    return b1 + b2 + b3 + b4;
}
