char  cifra_stotina(char s){
	if(s<100) {
return 48;
	} else {
return (char)(48+s / 100);
	}
}char  cifra_desetica(char s){
	if(s<10) {
return 48;
	} else {
return (char)(48+s / 10 % 10);
	}
}char  cifra_jedinica(char s){
return (char)(48+s % 10);
}int main() {
	char s[100] = {0};
	char t[100] = {0};
	char ascii;
	char tmp;
	int i;
	int j;
	int len;
	scanf("%s",&s);
	i = 1;
	j = 1;
	len = strlen(s);
while (i<=len){
		ascii = s[i-1];
i++;
		tmp = cifra_stotina(ascii);
		if(tmp != 48 || tmp == 48 && j>1) {
t[j-1]=tmp;
j++;
		}
t[j-1]=cifra_desetica(ascii);
j++;
t[j-1]=cifra_jedinica(ascii);
j++;
}
	printf("%s",t);
return 0;
}
