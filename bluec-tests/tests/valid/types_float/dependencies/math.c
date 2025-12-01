// include isnan macro and export a non-macro version we can use
#include <math.h>

int float_isnan(float f) {
    return isnan(f);
}

int float_isinf(float f) {
    return isinf(f);
}
