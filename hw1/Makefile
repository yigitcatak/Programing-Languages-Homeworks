flexyboi:
	flex ./yigitcatak-hw1.flx && gcc -o scanner lex.yy.c -lfl && ./scanner < deneme.JISP
orig:
	./hw1v3 < deneme.JISP
compare:
	((./hw1v3 < test1.JISP) > out1.txt & (./scanner < test1.JISP) > out2.txt) && diff out1.txt out2.txt
customcompare:
	((./hw1v3 < deneme.JISP) > out1.txt & (./scanner < deneme.JISP) > out2.txt) && diff out1.txt out2.txt