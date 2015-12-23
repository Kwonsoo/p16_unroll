/*
 * Bot 가 아니어서 diff에 나와야 하지만 diff 에서 사라지는 알람에 대한 예제
 */
typedef struct st {
	int number;
	char name[10];
} ST;

int main() {
	int name[10];
	int i;
	int j;
	//char *t;
	ST my_st;

	i = 0;
	i--;

	i = 0;
	if (i == j) {		// while문을 감싸고 있는 if-else 구문 없이 while문 블락이 main 안에서 바로 쓰였을 때는 diff 정상적으로 출력.
		;							// if 조건문이 i == j 대신 t==0 일 경우에도 정상적으로 diff 출력.
	} else {
		while (1) {
			if (! (i < 10))
				break;
      airac_print (i);
      airac_print (j);
			airac_observe(name, i);
			my_st.name[i] = name[i];	// dereference 지점.
			i++;
		}
	}
}
