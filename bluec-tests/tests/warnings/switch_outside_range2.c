int main(void) {
  char c = 1;

  switch (c) {
  case 0:
    return 1;
  case 1:
    return 0;
  case 128:
    return 2;
  case -129:
    return 2;
  default:
    return 3;
  }
}