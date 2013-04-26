/* procedure-structure:  Procedure structure. */

struct procedure {
  value *(*function) (struct procedure *, value **);
  size_t arity;
  value **environment;
  size_t environment_length;
};
