int inc(int i) { return i + 1; }

int main(){
  int x = 1;
  int y = x + 1;
  int z = y + 1;

  int a = lib();
  int b = inc(a);
  zoo_dump();
  return z;
}
