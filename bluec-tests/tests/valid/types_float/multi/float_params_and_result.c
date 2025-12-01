// Pass arguments of double type, including on stack, and return value of double type

float fmaxf(float x, float y); // from math.h

float get_max(float a, float b, float c, float d,
              float e, float f, float g, float h,
               // pass three arguments on the stack, make sure we adjust stack padding accordingly
              float i, float j, float k)
{

    float max = fmaxf(
        fmaxf(
            fmaxf(
                fmaxf(a, b),
                fmaxf(c, d)),
            fmaxf(
                fmaxf(e, f),
                fmaxf(g, h))),
        fmaxf(i, fmaxf(j, k)));
    return max;
}
