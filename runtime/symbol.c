/* symbol:  Symbol functions. */

#include <stdlib.h>
#include <string.h>
#include "value.h"
#include "symbol.h"
#include "predicate.h"
#include "error.h"
#include "memory.h"

/* SYMBOL_LIMIT:  Maximim amount of unique symbols. */
#define SYMBOL_LIMIT 128

/* symbol_counter:  Counts symbol identifiers. */
unsigned long symbol_counter = 0;

/* symbols:  Global symbol table, holds all symbols. */
struct symbol *symbols[SYMBOL_LIMIT];

/* find_symbol:  Look up a symbol by name. */
static struct symbol *find_symbol (char *name) {
  unsigned long i;

  for (i = 0; i < symbol_counter; i += 1)
    if (strcmp(name, symbols[i]->name) == 0)
      return symbols[i];

  return NULL;
}

/* next_symbol_identifier:  Return next ununsed symbol identifier and
   increment symbol_counter. */
static unsigned long next_symbol_identifier (void) {
  if (symbol_counter < SYMBOL_LIMIT) return symbol_counter++;
  else exit(OUT_OF_SYMBOLS);
}

/* new_symbol:  Returns a new symbol value for name. */
symbol *new_symbol (char *name) {
  symbol *sym  = (symbol *) allocate_value(SYMBOL);
  struct symbol *existing_symbol = find_symbol(name);
  unsigned long identifier, name_length = strlen(name);

  if (name_length == 0) exit(INVALID_SYMBOL_NAME);

  if (existing_symbol) sym->symbol = existing_symbol;
  else {
    identifier = next_symbol_identifier();
    symbols[identifier] = allocate_memory(sizeof(struct symbol));
    symbols[identifier]->identifier = identifier;
    symbols[identifier]->name = allocate_memory(sizeof(char)
						* (strlen(name) + 1));
    strcpy(symbols[identifier]->name, name);
    sym->symbol = symbols[identifier];
  }

  return sym;
}

/* symbolic_equality:  Predicate to test if two symbol values point to
   the same symbols. */
symbol *symbolic_equality (symbol *x, symbol *y) {
  ASSERT_TYPE(x, SYMBOL);
  ASSERT_TYPE(y, SYMBOL);

  return PREDICATE((x == NULL && y == NULL)
		   || ((x && y)
		       && x->symbol->identifier
		       == y->symbol->identifier));
}

/* symbol_p:  Predicate to test if a value is of type SYMBOL. */
symbol *symbol_p (value *object) {
  if (object == NULL) return T;
  else return value_type_p(object, SYMBOL);
}
