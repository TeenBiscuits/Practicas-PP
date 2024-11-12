#!/bin/bash

ocamlc -c hanoi_plus.mli hanoi_plus.ml
ocamlc -o test.o test.ml hanoi_plus.ml
ocamlrun ./test.o
