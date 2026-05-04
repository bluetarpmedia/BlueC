int main(void) {
  unsigned short us = 1;

  switch (us) {
  case 0:
    return 1;
  case 1:
    return 0;
  case 65536:
    return 2;
  default:
    return 3;
  }
}