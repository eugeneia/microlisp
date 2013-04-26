/* units-c:  A simple language to convert units in C.
   It reads queries in the form of
     value unit-identifier target-unit-identifiers...
   from stdin and prints conversion results of value from unit to target
   units. */

#include <stdlib.h>
#include <stdio.h>
#include <string.h>

#define UNIT_IDENTIFIER_LENGTH 255
#define UNITS_LENGTH 6
#define MAX_UNIT_IDENTIFIERS 5

/* enum unit:  Enumeration for units. */
enum unit { UNKNOWN, MM, CM, M, IN, FT };

/* unit:  Structured unit type.*/
typedef struct {
  float scale;
  unsigned unit_identifiers_n;
  char *unit_identifiers[MAX_UNIT_IDENTIFIERS];
} unit;

/* units:  Unit table. */
unit units[UNITS_LENGTH];

/* struct unit_identifier_cell:  Cell structure for list of unit_identifiers. */
struct unit_identifier_cell {
  char unit_identifier[UNIT_IDENTIFIER_LENGTH];
  struct unit_identifier_cell *next;
};

/* unit_identifiers:  Type for list of unit_identifiers. */
typedef struct unit_identifier_cell *unit_identifiers;

/* initialze_units:  Unit table initialization. */
void initialize_units (void) {
  units[MM].scale = 1;
  units[MM].unit_identifiers_n = 2;
  units[MM].unit_identifiers[0] = "millimeter";
  units[MM].unit_identifiers[1] = "mm";

  units[CM].scale = 10;
  units[CM].unit_identifiers_n = 2;
  units[CM].unit_identifiers[0] = "centimeter";
  units[CM].unit_identifiers[1] = "cm";

  units[M].scale = 1000;
  units[M].unit_identifiers_n = 2;
  units[M].unit_identifiers[0] = "meter";
  units[M].unit_identifiers[1] = "m";

  units[IN].scale = 25.4;
  units[IN].unit_identifiers_n = 3;
  units[IN].unit_identifiers[0] = "inch";
  units[IN].unit_identifiers[1] = "in";
  units[IN].unit_identifiers[2] = "zoll";

  units[FT].scale = 304.8;
  units[FT].unit_identifiers_n = 3;
  units[FT].unit_identifiers[0] = "feet";
  units[FT].unit_identifiers[1] = "ft";
  units[FT].unit_identifiers[2] = "fuß";
}

/* find_unit:  Find unit matching unit_identifier. Return index if found
   and -1 otherwise. */
enum unit find_unit (char *unit_identifier) {
  enum unit i;
  unsigned j;

  for (i = MM; i < UNITS_LENGTH; i += 1)
    for (j = 0; j < units[i].unit_identifiers_n; j += 1)
      if (strcmp(units[i].unit_identifiers[j], unit_identifier) == 0)
        return i;

  return UNKNOWN;
}

/* convert:  Convert value from unit specified by unit_index to unit
   specified by target_unit_index. */
float convert (float value,
               enum unit unit_index,
               enum unit target_unit_index) {
  return (value * units[unit_index].scale)
    / units[target_unit_index].scale;
}

/* interpret_query:  Interpret query (convert value from unit specified
   by unit_identifier to units specified by target_unit_identifiers) and
   print results. */
void interpret_query (float value, char *unit_identifier,
                      unit_identifiers target_unit_identifiers) {
  enum unit
    unit_index = find_unit(unit_identifier),
    target_unit_index;

  if (unit_index == UNKNOWN) {
    fprintf(stderr, "unknown unit identifier\n");
    exit(-1);
  }

  for (; target_unit_identifiers != NULL;
       target_unit_identifiers = target_unit_identifiers->next)
    if ((target_unit_index
         = find_unit(target_unit_identifiers->unit_identifier))
        == UNKNOWN) {
      fprintf(stderr, "unknown unit identifier\n");
    } else printf("%f %s\n",
                  convert(value, unit_index, target_unit_index),
                  target_unit_identifiers->unit_identifier);
}

int main (void) {
  float value;
  char
    unit_identifier[UNIT_IDENTIFIER_LENGTH],
    temp_unit_identifier[UNIT_IDENTIFIER_LENGTH];
  unit_identifiers
    target_unit_identifiers = NULL,
    head;
  
  initialize_units();
  
  if (scanf("%f %s", &value, unit_identifier) != 2) {
    fprintf(stderr, "invalid query\n");
    exit(-1);
  }

  while(scanf("%s", temp_unit_identifier) == 1) {
    if (target_unit_identifiers == NULL) {
      target_unit_identifiers = malloc(sizeof(unit_identifiers));
      head = target_unit_identifiers;
    } else {
      head->next = malloc(sizeof(unit_identifiers));
      head = head->next;
    }
    strcpy(head->unit_identifier, temp_unit_identifier);
  }
  head->next = NULL;
  
  if (target_unit_identifiers == NULL) {
    fprintf(stderr, "invalid query (no output units)\n");
    exit(-1);
  }

  interpret_query(value, unit_identifier, target_unit_identifiers);

  return 0;
}
