/* value-types:  Value types. */

struct procedure;
struct cell;
struct symbol;
struct number;
struct character;

enum type { PROCEDURE, CELL, SYMBOL, NUMBER, CHARACTER };

typedef struct {
  enum type type;
  struct procedure *procedure;
} procedure;

typedef struct {
  enum type type;
  struct cell *cell;
} cell;

typedef struct {
  enum type type;
  struct symbol *symbol;
} symbol;

typedef struct {
  enum type type;
  struct number *number;
} number;

typedef struct {
  enum type type;
  struct character *character;
} character;

union value {
  enum type type;
  procedure procedure;
  cell cell;
  symbol symbol;
  number number;
  character character;
};
  
typedef union value value;

#include "procedure-structure.h"
#include "cell-structure.h"
#include "symbol-structure.h"
#include "number-structure.h"
#include "character-structure.h"
