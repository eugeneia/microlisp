/* garbage:  Prototypes for garbage collection, depends on value.h. */

void use (value *);
void disuse (value *);
void collect_garbage (void);
