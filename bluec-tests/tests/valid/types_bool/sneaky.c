typedef unsigned long size_t;
void *memcpy(void *dst, void *src, size_t num_bytes);

int side_effect = 0;

_Bool get_true() { 
    side_effect++;
    return 1;
}

int main(void) {
    unsigned char invalid_value_for_bool = 2;
    _Bool forced_bool;
    memcpy(&forced_bool, &invalid_value_for_bool, 1);
    
    if (forced_bool != 0 && forced_bool != 1) {
        // This is technically UB, but a robust compiler should handle it.
        // If we return here, it means the compiler doesn't clamp/truncate when reading the bool.
        return 1;
    }

    _Bool b_inc = 1;
    b_inc++; 
    if (*(unsigned char*)&b_inc > 1) return 2;

    _Bool b1 = 1, b2 = 1;
    if ((b1 + b2) != 2) return 4;

    _Bool b_side = get_true();
    if (side_effect != 1) return 5;

    double neg_zero = -0.0;
    if ((_Bool)neg_zero != 0) return 6;

    int large_val = 256;
    if ((_Bool)large_val != 1) return 7;

    _Bool not_true = ~(_Bool)1;
    if (not_true == 0) return 8;

    return 0;
}