/* cell:  Prototypes for cell functions. */

cell *new_cell (value *, cell *);
value *first (cell *);
cell *rest (cell *);
symbol *cell_p (value *);
