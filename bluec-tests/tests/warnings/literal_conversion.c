void sink(_Bool b) { (void)b; }

_Bool a = 0.0; // No warning
_Bool b = 1.0; // No warning
_Bool c = -0.0;
_Bool d = -1.0;
_Bool e = 2.0;

int main(void) {
    sink(0.0f); // No warning
    sink(1.0f); // No warning
    sink(-0.0f);
    sink(-1.0f);
    sink(2.0f);

    return 0;
}