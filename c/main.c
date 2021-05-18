#include <stdio.h>
#include <inttypes.h>
#include <stdlib.h>

#define heap_size 1000000000 // 1 GB

int64_t entry();

void error() {
	printf("Error!\n");
	exit(EXIT_FAILURE);
}

int main(int argc, char** argv) {
	  void* heap = malloc(heap_size);
	  int64_t result = entry(heap);
	    printf("%" PRId64 "\n", result);
	      return 0;
}
