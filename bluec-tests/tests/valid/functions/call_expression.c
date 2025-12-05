int get(void) {
    return 16;
}

float add(float a, float b) {
    return a + b;
}

int main(void) {
    int a = get();
    int b = (get)();
    int c = ((get))();
    int d = (*get)();
    int e = (**********get)();

    float f = add(0.5f, 0.5f);
    float g = (add)(0.5f, 0.5f);
    float h = ((add))(0.5f, 0.5f);
    float i = (*add)(0.5f, 0.5f);
    float j = ((*add))(0.5f, 0.5f);
    float k = (*(*add))(0.5f, 0.5f);
    float l = (******add)(0.5f, 0.5f);

    if (a != 16) {
        return 1;
    }

    if (b != 16) {
        return 2;
    }

    if (c != 16) {
        return 3;
    }

    if (d != 16) {
        return 4;
    }

    if (e != 16) {
        return 5;
    }

    float sum = f + g + h + i + j + k + l;
    if (sum != 7.0f) {
        return 6;
    }

    return 0;
}