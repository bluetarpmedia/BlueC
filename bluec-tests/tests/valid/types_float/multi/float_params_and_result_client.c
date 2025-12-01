float get_max(float a, float b, float c, float d,
              float e, float f, float g, float h,
              float i, float j, float k);

int main(void)
{
    float result = get_max(100.3f, 200.1f, 0.01f, 1.00004e5f, 55.555f, -4.f, 6543.2f,
                            9e9f, 8e8f, 7.6f,  10e3f * 11e5f);
    return result == 10e3f * 11e5f;
}