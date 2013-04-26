/* io:  Input/output functions (ASCII only). */

#include <stdlib.h>
#include <string.h>
#include <stdio.h>
#include "value.h"
#include "cell.h"
#include "symbol.h"
#include "number.h"
#include "character.h"
#include "predicate.h"
#include "memory.h"
#include "error.h"
#include "garbage.h"
#include "io.h"

/* WHITESPACE_P:  Predicate to test if c is a whitespace character. */
#define WHITESPACE_P(c) (c == ' ' || c == '\n' || c == '\t')

#define ASCII_SIZE 128
#define PATH_SIZE 2048
#define SYMBOL_NAME_SIZE 256

enum mode { READ, WRITE };

enum list_type { MIXED, STRING };

static unsigned char parse_error_p;

/* character_to_char:  Convert character to char (ASCII). */
static char character_to_char (character *c) {
  unsigned long code = c->character->code;

  if (code < ASCII_SIZE) return (char) code;
  else exit(INVALID_IO_UNPRINTABLE);
}

/* list_length:  Returns number of list items in list. */
static unsigned long list_length (cell *list) {
  unsigned long length = 0;

  while (list != NULL) {
    length += 1;
    list = list->cell->rest;
  }

  return length;
}

/* resource_path:  Returns path for resource
   (list of lists of characters). */
static char *resource_path (cell *resource) {
  unsigned int i, path_length = list_length(resource),
    path_character_length;
  cell *head, *head2;
  value *object;
  char *path;

  if (path_length > PATH_SIZE) exit(INVALID_IO_RESOURCE);

  for (head = resource, path_character_length = 0;
       head != NULL; head = head->cell->rest) {
    object = head->cell->first;
    if (!cell_p(object)) exit(INVALID_IO_RESOURCE);
    path_character_length += list_length((cell *) object);
  }

  if (path_character_length > PATH_SIZE) exit(INVALID_IO_RESOURCE);

  path = allocate_memory(sizeof(char) // path characters + slashes
                         * (path_length + path_character_length));
  
  for (i = 0, head = resource; head != NULL; head = head->cell->rest) {
    object = head->cell->first;

    if (!cell_p(object)) exit(INVALID_IO_RESOURCE);

    for (head2 = (cell *) object;
         head2 != NULL;
         head2 = head2->cell->rest, i+= 1) {
      object = head2->cell->first;

      if (!character_p(object)) exit(INVALID_IO_RESOURCE);
      
      path[i] = character_to_char((character *) object);
    }
    
    if (head->cell->rest != NULL) path[i] = '/', i += 1;
  }

  path[i] = '\0';

  return path;
}

/* open_resource_stream:  Returns stream for resource opened with
   specified mode. */
static FILE *open_resource_stream (cell *resource, enum mode m) {
  char *path;
  FILE *stream;

  if (m != READ && m != WRITE) exit(RUNTIME_CORRUPTION); 

  if (resource == NIL) switch (m) {
    case READ: return stdin;
    case WRITE: return stdout;
    }

  path = resource_path((cell *) resource);

  stream = fopen(path, m == READ ? "r" : "w");

  free_memory(path);

  return stream;
}

/* close_resource_stream:  Close a resource stream unless it is stdin or
   stdout. */
static void close_resource_stream (FILE *stream) {
  if (stream != stdin && stream != stdout)
    fclose(stream);
}

/* get_nonwhite_c:  Return next non-whitespace character in stream. */
int get_nonwhite_c (FILE *stream) {
  int c;

  do c = getc(stream); while (WHITESPACE_P(c));

  return c;
}

static value *read_value (FILE *);

/* read_list:  Return a list of type read from stream. */
static cell *read_list (FILE *stream, enum list_type type) {
  int c, escape_p = 0;
  cell *list = NULL, *item;
  value *object;

  if (type == MIXED) {    
    do {
      c = get_nonwhite_c(stream);
      
      if (c == EOF) {
        parse_error_p = 1;
	disuse((value *) list);
        return NULL;
      }
      if (c == ')') return list;

      ungetc(c, stream);
      
      object = read_value(stream);

      if (parse_error_p) return NULL;

      if (list == NULL) {
        list = new_cell(object, NULL);
        item = list;
      } else {
        item->cell->rest = new_cell(object, NULL);
        item = item->cell->rest;
	use((value *) item);
      }
    } while (1);
    
  } else if (type == STRING) {
    do {
      c = getc(stream);

      if (c == EOF) {
        parse_error_p = 1;
        disuse((value *) list);
        return NULL;
      }
      if (c == '"' && !escape_p) return list;
      if (c == '\\' && !escape_p) {
        escape_p = 1;
        continue;
      }

      if (list == NULL) {
        list = new_cell((value *) new_character(c), NULL);
        item = list;
      } else {
        item->cell->rest = new_cell((value *) new_character(c), NULL);
        item = item->cell->rest;
	use((value *) item);
      }
    
      escape_p = 0;
    } while (1);
  } else exit(RUNTIME_CORRUPTION);

  return NULL; // anti warning
}

static unsigned char print_value (value *, FILE *);

/* print_list:  Print list to stream. */
static unsigned char print_list (cell *list, FILE *stream) {
  cell *head;
  enum list_type type;
  char c;

  for (type = STRING, head = list; head != NULL; head = head->cell->rest)
    if (!character_p(head->cell->first)) {
      type = MIXED;
      break;
    }

  if (type == STRING) {
    if (0 > putc('"', stream)) return 0;
    
    for (head = list; head != NULL; head = head->cell->rest) {
      c = character_to_char((character *) head->cell->first);

      if (c == '"' && 0 > putc('\\', stream)) return 0;
      if (0 > putc(((character *) head->cell->first)->character->code,
		   stream))
	return 0;
    }
    
    if (0 > putc('"', stream)) return 0;

  } else {
    if (0 > putc('(', stream)) return 0;

    for (head = list; head != NULL;) {
      if (!print_value(head->cell->first, stream)) return 0;

      head = head->cell->rest;

      if (head != NULL && 0 > putc(' ', stream)) return 0;
    }

    if (0 > putc(')', stream)) return 0;
  }  
 
  return 1;
}

/* read_symbol:  Return symbol read from stream. */
static symbol *read_symbol (FILE *stream) {
  int i, c;
  char name[SYMBOL_NAME_SIZE];

  for (i = 0;; i += 1) {
    if (i > SYMBOL_NAME_SIZE) exit(INVALID_IO_UNPRINTABLE);

    c = getc(stream);
    
    if (c == EOF || WHITESPACE_P(c) || c == ')') break;
    else name[i] = c;
  }
  name[i] = '\0';

  ungetc(c, stream);

  return new_symbol(name);
}

/* print_symbol:  Print symbol sym to stream. */
static unsigned char print_symbol (symbol *sym, FILE *stream) {
  if (sym == NIL) return 0 < fprintf(stream, "NIL");

  if (strlen(sym->symbol->name) > SYMBOL_NAME_SIZE)
    exit(INVALID_IO_UNPRINTABLE);

  else return 0 < fprintf(stream, "%s", sym->symbol->name);
}

/* read_number:  Return a number read from stream. */
static number *read_number (FILE *stream) {
  long numerator;
  unsigned long denominator;
  int result;

  result = fscanf(stream, "%ld/%ld", &numerator, &denominator);

  if (result < 1) {
    parse_error_p = 1;
    return NULL;
  }
  
  if (result == 1) return new_number(numerator, 1);
  else             return new_number(numerator, denominator);
}

/* print_number:  Print number n to stream. */
static unsigned char print_number (number *n, FILE *stream) {
  int result;

  if (n->number->denominator == 1)
    result = fprintf(stream, "%ld", n->number->numerator);
  else result = fprintf(stream, "%ld/%ld",
			n->number->numerator,
			n->number->denominator);

  return 0 < result;
}

/* read_character:  Return a character read from stream. */
static character *read_character (FILE *stream) {
  int c = getc(stream), terminator;
  
  if (c == EOF || (terminator = getc(stream)) != '\'') {
    parse_error_p = 1;
    return NULL;

  } else return new_character(c);
}

/* print_character:  Print character c to stream. */
static unsigned char print_character (character *c, FILE *stream) {
  return 0 < fprintf(stream, "'%c'", character_to_char(c));
}

/* read_value:  Return a value read from stream. */
static value *read_value (FILE *stream) {
  int c;
  
  if (parse_error_p) return NULL;

  c = get_nonwhite_c(stream);

  switch (c) {
  case EOF: case ')':
    parse_error_p = 1;
    return NULL;
  case '(': return (value *) read_list(stream, MIXED);
  case '"': return (value *) read_list(stream, STRING);
  case '\'': return (value *) read_character(stream);
  case '-': case '0': case '1': case '2': case '3': case '4':
  case '5': case '6': case '7': case '8': case '9':
    ungetc(c, stream);
    return (value *) read_number(stream);
  default:
    ungetc(c, stream);
    return (value *) read_symbol(stream);
  }
}
  
/* print_value:  Print value object to stream. */
static unsigned char print_value (value *object, FILE *stream) {
  if (object == NIL) return print_symbol(NIL, stream);

  switch (object->type) {
  case PROCEDURE: exit(INVALID_IO_UNPRINTABLE);
  case CELL:      return print_list((cell *) object, stream);
  case SYMBOL:    return print_symbol((symbol *) object, stream);
  case NUMBER:    return print_number((number *) object, stream);
  case CHARACTER: return print_character((character *) object, stream);
  default:        exit(RUNTIME_CORRUPTION);
  }
}

/* read:  Read a value from resource, return value if successful and NIL
   otherwise. */
value *read (cell *resource) {
  FILE *stream;
  value *object;

  if (resource) ASSERT_TYPE(resource, CELL);

  if ((stream = open_resource_stream(resource, READ)) == NULL)
    return NIL;

  parse_error_p = 0;

  object = read_value(stream);

  close_resource_stream(stream);

  return object;
}

/* write:  Write a value to resource in readable form, return T if
   successful and NIL otherwise. */
symbol *write (value *object, cell *resource) {
  FILE *stream;

  if (resource) ASSERT_TYPE(resource, CELL);

  if ((stream = open_resource_stream(resource, WRITE)) == NULL
      || !print_value(object, stream))
    return NIL;
  
  fprintf(stream, "\n");
  fflush(stream);
  
  close_resource_stream(stream);
  
  return T;
}

/* delete:  Delete resource, return T if successful, NIL on failure. */
symbol *delete (cell *resource) {
  ASSERT_TYPE(resource, CELL);

  if (remove(resource_path(resource)) == 0) return T;
  else return NIL;
}
