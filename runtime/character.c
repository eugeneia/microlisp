/* character:  Character functions. */

#include <stdlib.h>
#include "value.h"
#include "character.h"
#include "symbol.h"
#include "predicate.h"
#include "memory.h"
#include "error.h"

/* new_character:  Returns a new character value by code. */
character *new_character (unsigned long code) {
  character *c = (character *) allocate_value(CHARACTER);

  c->character = allocate_memory(sizeof(struct character));
  c->character->code = code;

  return c;
}


/* characteristic_equality:  Predicate to test if two character values
   represent equal characters. */
symbol *characteristic_equality (character *character_a,
				 character *character_b) {
  ASSERT_TYPE(character_a, CHARACTER);
  ASSERT_TYPE(character_b, CHARACTER);

  return PREDICATE(character_a->character->code
		   == character_b->character->code);
}

/* character_p:  Predicate to test if value is of type CHARACTER. */
symbol *character_p (value *object) {
  return value_type_p(object, CHARACTER);
}
