extern void *x;

void foo(void) { return; }

int main(void) {
  *x = foo();
  return 0;
}