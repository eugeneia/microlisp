/* cell:  Cell functions. */

#include <stdlib.h>
#include "value.h"
#include "cell.h"
#include "memory.h"
#include "garbage.h"

/* new_cell:  Constructs a new cell with values first and rest. */
cell *new_cell (value *first, cell *rest) {
  cell *c = (cell *) allocate_value(CELL);

  if (rest) ASSERT_TYPE(rest, CELL);
  
  c->cell = allocate_memory(sizeof(struct cell));

  c->cell->first = first;
  c->cell->rest  = rest;

  use(first);
  use((value *) rest);
  
  return c;
}

/* first:  Returns first slot of cell. */
value *first (cell *c) {
  ASSERT_TYPE(c, CELL);

  return c->cell->first;
}

/* rest:  Returns rest slot of cell. */
cell *rest (cell *c) {
  ASSERT_TYPE(c, CELL);

  return c->cell->rest;
}

/* cell_p:  Predicate to test if value is of type CELL. */
symbol *cell_p (value *object) {
  return value_type_p(object, CELL);
}
