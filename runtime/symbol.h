/* symbol:  Constants and prototypes for symbol functions. */

#define T new_symbol("T")
#define NIL NULL

symbol *new_symbol (char *);
symbol *symbolic_equality (symbol *, symbol *);
symbol *symbol_p (value *);
