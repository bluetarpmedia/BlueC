int main(void) {
  _Bool b = 1;

  switch (b) {
  case 0:
    return 1;
  case 1:
    return 0;
  case 2:
    return 2;
  default:
    return 3;
  }
}