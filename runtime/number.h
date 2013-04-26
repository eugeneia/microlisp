/* number:  Prototypes for number functions. */

number *new_number (long, unsigned long);
symbol *number_p (value *);
symbol *numeric_equality (number *, number *);
symbol *numeric_greater (number *, number *);
number *add_number (number *, number *);
number *subtract_number (number *, number *);
number *modulo_number (number *, number *);
number *multiply_number (number *, number *);
number *divide_number (number *, number *);
