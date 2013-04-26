/* memory: Memory functions. */

#include <stdlib.h>
#include "memory.h"
#include "error.h"

/* allocate_memory:  Returns pointer to bytes allocated from heap. */
void *allocate_memory (size_t bytes) {
  void *pointer = malloc(bytes);

  if (pointer) return pointer;
  else exit(OUT_OF_MEMORY);
}

/* free_memory:  Frees memory designated by pointer. */
void free_memory (void *pointer) {
  free(pointer);
}
