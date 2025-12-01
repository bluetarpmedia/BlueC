int main(void) {
    typedef float MyFloat, Float32, F32;
    float typedef MyFloat, Float32, F32;

    MyFloat a = 1.0f;
    Float32 b = 2.0f;
    F32     c = 3.0f;
    
    return (a + b + c == 6.0f);
}
