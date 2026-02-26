_Bool static a = 1;
static _Bool b = 0;
_Bool c = 1;

int main(void)
{
    extern _Bool a;
    _Bool extern b;
    extern _Bool c;

    if (a != 1) {
        return 1;
    }

    if (b != 0) {
        return 2;
    }

    if (c != 1) {
        return 3;
    }

    return 0;
}