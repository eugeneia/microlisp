/* procedure:  Prototypes for procedure functions. */

procedure *new_procedure (struct procedure *, value **,
			  value *(*) (struct procedure *, value **),
			  size_t);
value *call_procedure (procedure *, ...);
symbol *procedure_p (value *);
