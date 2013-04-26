/* procedure:  Procedure functions. */

#include <stdlib.h>
#include <stdarg.h>
#include <string.h>
#include "value.h"
#include "procedure.h"
#include "memory.h"
#include "garbage.h"

/* append_environment:  Copies concatenation of parent environment and 
   arguments into environment of procedure. */
static void append_environment (struct procedure *parent,
                                value **parent_arguments,
                                struct procedure *procedure) {
  size_t environment_length = 0;
  value **environment;

  if (parent)
    environment_length = parent->environment_length + parent->arity;

  if (environment_length) {
    environment = allocate_memory(sizeof(value *) * environment_length);
    memcpy(environment, parent_arguments,
	   sizeof(value *) * parent->arity);
    memcpy(&environment[parent->arity], parent->environment,
           sizeof(value *) * parent->environment_length);
  } else environment = NULL;

  procedure->environment = environment;
  procedure->environment_length = environment_length;
}

/* new_procedure:  Returns a new procedure for function with arity. */
procedure *new_procedure (struct procedure *parent,
			  value **parent_arguments,
			  value *(*function)
			  (struct procedure *, value **),
			  size_t arity) {
  size_t i;
  procedure *proc = (procedure *) allocate_value(PROCEDURE);

  proc->procedure = allocate_memory(sizeof(struct procedure));
  
  proc->procedure->function = function;
  proc->procedure->arity = arity;
  append_environment(parent, parent_arguments, proc->procedure);

  for (i = 0; i < proc->procedure->environment_length; i += 1)
    use(proc->procedure->environment[i]);

  return proc;
}

/* call_procedure:  Call a procedure with arguments. */
value *call_procedure (procedure *proc, ...) {
  size_t i;
  va_list argument_list;
  value *result, **arguments = NULL;

  ASSERT_TYPE(proc, PROCEDURE);

  use((value *) proc);

  if (proc->procedure->arity > 0) {
    arguments = allocate_memory(sizeof(value *)
				* proc->procedure->arity);
    
    va_start(argument_list, proc);
    for (i = 0; i < proc->procedure->arity; i += 1) {
      arguments[i] = va_arg(argument_list, value *);
      use(arguments[i]);
    }
    va_end(argument_list);
  }

  collect_garbage();
  
  result = proc->procedure->function(proc->procedure, arguments);

  for (i = 0; i < proc->procedure->arity; i += 1)
    disuse(arguments[i]);

  disuse((value *) proc);
  
  free_memory(arguments);

  return result;
}

/* procedure_p:  Predicate to test if a value is of type PROCEDURE. */
symbol *procedure_p (value *object) {
  return value_type_p(object, PROCEDURE);
}
