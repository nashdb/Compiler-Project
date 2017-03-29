int g(int x, int y) {
  x = 2 + 4 + 6;
  return x + y;
  }
  
int f(int x, int y) {
  x = 2 + 4 + 6;
  y = g(4 * 8, 6 * 10);
  x = 2 + 2;
  }
