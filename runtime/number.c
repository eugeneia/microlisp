/* number:  Number functions. */

#include <stdlib.h>
#include "value.h"
#include "number.h"
#include "symbol.h"
#include "predicate.h"
#include "memory.h"

#define SIGN(x) ((x) < 0 ? -1 : 1)

/* optimize_number:  Minimize numerator and denominator of number
   (destructive). */
static void optimize_number (struct number *number) {
  unsigned long i;
  char z_sign = SIGN(number->numerator);

  number->numerator *= z_sign; // strip sign of number->numerator

  for (i = number->denominator; i > 0; i -= 1) {
    if (number->numerator % i == 0
        && number->denominator % i == 0) {
      number->numerator /= i;
      number->denominator /= i;
    }
  }

  number->numerator *= z_sign; // add sign back to number->numerator
}

/* new_number:  Returns a real number defined by numerator and
   denominator. */
number *new_number (long numerator, unsigned long denominator) {
  number *num = (number *) allocate_value(NUMBER);

  num->number = allocate_memory(sizeof(struct number));
  num->number->numerator = numerator;
  num->number->denominator = denominator;

  optimize_number(num->number);

  return num;
}

/* number_p:  Predicate to test if value is of type NUMBER. */
symbol *number_p (value *object) {
  return value_type_p(object, NUMBER);
}

/* numeric_equality: Predicate to test if numbers a and b are
   numerically equal. */
symbol *numeric_equality (number *number_a, number *number_b) {
  number *num = subtract_number(number_a, number_b);
  symbol *result = PREDICATE(num->number->numerator == 0);

  free_value((value *) num);
  
  return result;
}

/* numeric_greater:  Predicate to test if number_a is greater than
   number_b. */
symbol *numeric_greater(number *number_a, number *number_b) {
  return PREDICATE((number_a->number->numerator
		    / number_a->number->denominator)
		   > (number_b->number->numerator
		      / number_b->number->denominator));
}

/* add_number:  Returns number representing the sum of number_a and
   number_b. */
number *add_number (number *number_a, number *number_b) {
  ASSERT_TYPE(number_a, NUMBER);
  ASSERT_TYPE(number_b, NUMBER);

  return new_number((number_a->number->numerator
		     * number_b->number->denominator)
		    + (number_b->number->numerator
		       * number_a->number->denominator),
		    number_b->number->denominator
		    * number_b->number->denominator);
}

/* subtract_number:  Returns number representing the difference of
   number_a and number_b. */
number *subtract_number (number *number_a, number *number_b) {
  ASSERT_TYPE(number_a, NUMBER);
  ASSERT_TYPE(number_b, NUMBER);

  return new_number((number_a->number->numerator
		     * number_b->number->denominator)
		    - (number_b->number->numerator
		       * number_a->number->denominator),
		    number_b->number->denominator
		    * number_b->number->denominator);
}

/* modulo_number:  Returns number representing*/
number *modulo_number (number *number_a, number *number_b) {
  ASSERT_TYPE(number_a, NUMBER);
  ASSERT_TYPE(number_b, NUMBER);

  return new_number((number_a->number->numerator
		     * number_b->number->denominator)
		    % (number_b->number->numerator
		       * number_a->number->denominator),
		    number_b->number->denominator
		    * number_b->number->denominator);
}

/* multiply_number:  Returns number representing the product of number_a
   and number_b. */
number *multiply_number (number *number_a, number *number_b) {
  ASSERT_TYPE(number_a, NUMBER);
  ASSERT_TYPE(number_b, NUMBER);

  return new_number(number_a->number->numerator
                    * number_b->number->numerator,
                    number_a->number->denominator
                    * number_b->number->denominator);
}

/* divide_number:  Returns number representing the quotient of number_a
   and number_b. */
number *divide_number (number *number_a, number *number_b) {
  ASSERT_TYPE(number_a, NUMBER);
  ASSERT_TYPE(number_b, NUMBER);
  
  return new_number(number_a->number->numerator
                    * number_b->number->denominator,
                    number_a->number->denominator
                    * number_b->number->numerator);
}
