compile:
	flex yigitcatak-hw2.flx && bison -d yigitcatak-hw2.y && gcc -o yigitcatak-hw2 lex.yy.c yigitcatak-hw2.tab.c -lfl 

run: 
	./yigitcatak-hw2 < deneme.JISP
	
orig:
	./hw2v3 < deneme.JISP

clear:
	rm ./lex.yy.c & rm ./yigitcatak-hw2.tab.c & rm ./yigitcatak-hw2.tab.h & clear