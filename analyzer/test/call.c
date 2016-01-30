void f(){
  g();
}

void g(){
}

void main(){
  f();
  rec();
}


void rec(){
  rec1();
}

void rec1(){
  rec2();
}

void rec2(){
  rec();
}
