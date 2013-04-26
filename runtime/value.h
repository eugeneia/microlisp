/* value:  Value types and prototypes for value functions. */

#include "value-types.h"

#define ASSERT_TYPE(object, type)\
  assert_value_type((value *) object, type)

value *allocate_value (enum type);
void free_value (value *);
symbol *value_type_p (value *, enum type);
void assert_value_type (value *, enum type);
