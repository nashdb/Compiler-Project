int f(int x) {
    return x + 1;
}

int g(int a, int b) {
  b = f(a * 2);
  return b;
}
