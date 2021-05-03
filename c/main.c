#include <stdio.h>
#include <inttypes.h>
#include <stdlib.h>

#define heap_size 100000

int64_t entry();

void error() {
	printf("Error!\n");
	exit(EXIT_FAILURE);
}

int main(int argc, char** argv) {
	  void* heap = malloc(heap_size);
	  int64_t result = entry(heap_size);
	    printf("%" PRId64 "\n", result);
	      return 0;
}
