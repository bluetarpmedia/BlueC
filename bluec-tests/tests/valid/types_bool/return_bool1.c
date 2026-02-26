_Bool is_greater_than_one(float value) {
    return value > 1.0f;
}

int main(void) {
    if (!is_greater_than_one(2.0f)) {
        return 1;
    }
    
    _Bool result = is_greater_than_one(1.1f);
    if (!result) {
        return 2;
    }

    if (is_greater_than_one(0.5f) != 0) {
        return 3;
    }

    return 0;
}