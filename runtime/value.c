/* value:  Value functions. */

#include <stdlib.h>
#include "memory.h"
#include "value.h"
#include "cell.h"
#include "symbol.h"
#include "error.h"
#include "garbage.h"
#include "predicate.h"

/* allocate_value:  Returns pointer to allocated memory for an
   initialized value of type. */
value *allocate_value (enum type type) {
  value *value;

  value = allocate_memory(sizeof(value));

  value->type = type;

  return value;
}

/* free_value:  Free memory used by value (rather smart). */
void free_value (value *object) {
  size_t i;

  if (object == NIL) return;

  switch (object->type) {

  case PROCEDURE:
    for (i = 0;
	 i < object->procedure.procedure->environment_length;
	 i += 1)
      disuse(object->procedure.procedure->environment[i]);
    
    free_memory(object->procedure.procedure->environment);
    free_memory(object->procedure.procedure);
    break;

  case CELL:
    disuse(((cell *) object)->cell->first);
    disuse((value *) ((cell *) object)->cell->rest);

    free_memory(object->cell.cell);
    break;

  case SYMBOL:
    break;

  case NUMBER:
    free_memory(object->number.number);
    break;

  case CHARACTER:
    free_memory(object->character.character);
    break;

  default:
    exit(RUNTIME_CORRUPTION);
  }
  free_memory(object);
}

/* value_type_p:  Genereic type checking predicate. */
symbol *value_type_p (value *object, enum type object_type) {
  if (object == NIL) return PREDICATE(object_type == SYMBOL);
  else               return PREDICATE(object->type == object_type);
}

/* assert_value_type:  Runtime type assertion. */
void assert_value_type (value *object, enum type object_type) {
  if (!value_type_p(object, object_type)) exit(INVALID_TYPE);
}
