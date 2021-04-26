#!/bin/bash

gcc -c ./c/main.c -o ./build/main.o
raco exe -o ./build/racket ./racket/main.rkt
