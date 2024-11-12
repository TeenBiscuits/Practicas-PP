all: 
	ocamlc -c hanoi_plus.mli hanoi_plus.ml
	ocamlc -o test.o hanoi_plus.ml test.ml
	ocamlrun ./test.o

clean:
	rm *.cmi *.cmo *.o