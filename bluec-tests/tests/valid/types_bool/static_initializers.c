int x = 1;
int arr[3] = {1, 2, 3};
int calc(void) { return 1; }

_Bool t1 = 1;
_Bool t2 = &x;
_Bool t3 = &arr[1];
_Bool t4 = &calc;
_Bool t5 = (void*)10;
_Bool t6 = "test";
_Bool t7 = !(0);
_Bool t8 = !(10 - 10);

_Bool f1 = 0;
_Bool f2 = (void*)0;
_Bool f3 = !1;
_Bool f4 = !(0 + 10);

int main(void) {
    _Bool true_values = t1 && t2 && t3 && t4 && t5 && t6 && t7 && t8;
    _Bool false_values = f1 || f2 || f3 || f4;

    if (!true_values) {
        return 1;
    }

    if (false_values) {
        return 2;
    }

    return 0;
}
