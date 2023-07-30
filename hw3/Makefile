compile:
	(flex yigitcatak-hw3.flx && bison -d yigitcatak-hw3.y && gcc -o yigitcatak-hw3 lex.yy.c yigitcatak-hw3.tab.c -lfl) && ./yigitcatak-hw3 < deneme.JISP
	
orig:
	./hw3v3 < deneme.JISP

compare:
	((./hw3v3 < test3.JISP) > out1.txt & (./hw3v3 < test3.JISP) > out2.txt) && diff out1.txt out2.txt

clear:
	rm ./lex.yy.c & rm ./yigitcatak-hw3.tab.c & rm ./yigitcatak-hw3.tab.h & rm ./yigitcatak-hw3 & rm ./out1.txt & rm ./out2.txt ; clear