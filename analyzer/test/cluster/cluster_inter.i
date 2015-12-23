
int* globalx;
int arr[10];

int f(int* x){
  if (!x) {
    globalx = x;
  }
  else {
    globalx=malloc(g());
  }
  *(globalx)=2;
}

int h(){
  return *(globalx);
}

int main(){
  globalx=malloc(g());
  h();
  f(0);
  *(globalx) =1;
  f(globalx);
  *(globalx)=2;
}
