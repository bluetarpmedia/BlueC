int check_args(_Bool a, _Bool b, _Bool c, _Bool d, _Bool e, _Bool f, _Bool g, _Bool h);

int main(void) {
    _Bool a = 1;
    _Bool b = 0;
    _Bool c = 1;
    _Bool d = 0;
    _Bool e = 1;
    _Bool f = 0;
    _Bool g = 1;
    _Bool h = 0;
    
    return check_args(a, b, c, d, e, f, g, h);
}