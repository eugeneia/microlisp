/* garbage:  Garbage collection by reference counting. */

#include <stdlib.h>
#include "memory.h"
#include "value.h"
#include "error.h"

#define HASH_SIZE 127 // YES MATE, A FUCKIN' PRIME NUMBER!
#define REFERENCE_COUNT_MAX 128

/* reference_node:  Data structure to store reference counts. */
struct reference_node {
  value *pointer;
  unsigned char count;
  struct reference_node *next;
};

/* reference_table:  Hash table that holds all reference_nodes. */
static struct reference_node *reference_table[HASH_SIZE];

/* garbage_present:  Flag that indicates if garbage is present. */
static unsigned char garbage_present = 0;

/* new_reference:  Returns new reference_node for pointer. */
static struct reference_node *new_reference (value *pointer) {
  struct reference_node *node
    = allocate_memory(sizeof(struct reference_node));

  node->pointer = pointer;
  node->count = 1;
  node->next = NULL;
  
  return node;
}

/* pointer_hash:  Return hash for pointer. (Cheap as fook hash
   function.) */
static unsigned int pointer_hash (value *pointer) {
  return (unsigned long int) pointer % HASH_SIZE;
}

/* lookup:  Returns reference_node for pointer in reference_table if
   present. */
static struct reference_node *lookup (value *pointer) {
  struct reference_node *node = reference_table[pointer_hash(pointer)];
  
  for (; node != NULL; node = node->next)
    if (node->pointer == pointer) return node;
  
  return NULL;
}

/* install:  Install reference_node new_node in reference_table. */
static void install (struct reference_node *new_node) {
  unsigned int hash = pointer_hash(new_node->pointer);
  struct reference_node *node = reference_table[hash];
  
  if (node) {
    while (node->next != NULL) node = node->next;
    node->next = new_node;
  } else reference_table[hash] = new_node;
}

/* free_garbage_nodes:  Free all reference_nodes and their associated
   pointers in node if their count is zero. */
static struct reference_node *free_garbage_nodes
(struct reference_node *node) {
  struct reference_node *next;
  
  if (node == NULL) return NULL;

  if (node->count == 0) {
    free_value(node->pointer);
    next = node->next;
    free_memory(node);
    return free_garbage_nodes(next);

  } else {
    node->next = free_garbage_nodes(node->next);
    return node;
  }
}

/* use:  Increment reference count for pointer. */
void use (value *pointer) {
  struct reference_node *node;

  if (pointer == NULL) return;

  node = lookup(pointer);

  if (node != NULL) {
    if (node->count < REFERENCE_COUNT_MAX) node->count += 1;
    else exit(REFERENCE_OVERFLOW);
  } else install(new_reference(pointer));
}

/* disuse:  Decrement reference count for pointer. */
void disuse (value *pointer) {
  if (pointer == NULL) return;

  if ((lookup(pointer)->count -= 1) == 0)
    garbage_present = 1;
}

/* collect_garbage:  Free present garbage. */
void collect_garbage (void) {
  unsigned int i;

  while (garbage_present) {
    garbage_present = 0;
    for (i = 0; i < HASH_SIZE; i += 1)
      reference_table[i] = free_garbage_nodes(reference_table[i]);
  }
}
