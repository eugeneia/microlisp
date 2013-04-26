#include <stdlib.h>
#include "runtime.h"
value *global_environment[17];
struct procedure *proc = NIL;
value **arguments = NIL;
static value *f0 (struct procedure *proc, value **arguments) {
return (value *) (arguments[0] ? (value *) arguments[0] : (global_environment[1] ? (value *) global_environment[0] : NIL ) );
}
static value *f1 (struct procedure *proc, value **arguments) {
return (value *) (arguments[0] ? (value *) arguments[0] : NIL );
}
static value *f2 (struct procedure *proc, value **arguments) {
return (value *) (arguments[0] ? (value *) call_procedure((procedure *) new_procedure(proc, arguments, f1, 1), (value *) characteristic_equality((character *) proc->environment[4], (character *) proc->environment[5])) : NIL );
}
static value *f3 (struct procedure *proc, value **arguments) {
return (value *) (arguments[0] ? (value *) call_procedure((procedure *) new_procedure(proc, arguments, f2, 1), (value *) character_p((value *) proc->environment[4])) : NIL );
}
static value *f4 (struct procedure *proc, value **arguments) {
return (value *) (arguments[0] ? (value *) arguments[0] : (global_environment[1] ? (value *) call_procedure((procedure *) new_procedure(proc, arguments, f0, 1), (value *) call_procedure((procedure *) new_procedure(proc, arguments, f3, 1), (value *) character_p((value *) proc->environment[2]))) : NIL ) );
}
static value *f5 (struct procedure *proc, value **arguments) {
return (value *) (arguments[0] ? (value *) arguments[0] : NIL );
}
static value *f6 (struct procedure *proc, value **arguments) {
return (value *) (arguments[0] ? (value *) call_procedure((procedure *) new_procedure(proc, arguments, f5, 1), (value *) numeric_equality((number *) proc->environment[3], (number *) proc->environment[4])) : NIL );
}
static value *f7 (struct procedure *proc, value **arguments) {
return (value *) (arguments[0] ? (value *) call_procedure((procedure *) new_procedure(proc, arguments, f6, 1), (value *) number_p((value *) proc->environment[3])) : NIL );
}
static value *f8 (struct procedure *proc, value **arguments) {
return (value *) (arguments[0] ? (value *) arguments[0] : (global_environment[1] ? (value *) call_procedure((procedure *) new_procedure(proc, arguments, f4, 1), (value *) call_procedure((procedure *) new_procedure(proc, arguments, f7, 1), (value *) number_p((value *) proc->environment[1]))) : NIL ) );
}
static value *f9 (struct procedure *proc, value **arguments) {
return (value *) (arguments[0] ? (value *) arguments[0] : NIL );
}
static value *f10 (struct procedure *proc, value **arguments) {
return (value *) (arguments[0] ? (value *) call_procedure((procedure *) new_procedure(proc, arguments, f9, 1), (value *) symbolic_equality((symbol *) proc->environment[2], (symbol *) proc->environment[3])) : NIL );
}
static value *f11 (struct procedure *proc, value **arguments) {
return (value *) (arguments[0] ? (value *) call_procedure((procedure *) new_procedure(proc, arguments, f10, 1), (value *) symbol_p((value *) proc->environment[2])) : NIL );
}
static value *f12 (struct procedure *proc, value **arguments) {
return (value *) (arguments[0] ? (value *) arguments[0] : (global_environment[1] ? (value *) call_procedure((procedure *) new_procedure(proc, arguments, f8, 1), (value *) call_procedure((procedure *) new_procedure(proc, arguments, f11, 1), (value *) symbol_p((value *) proc->environment[0]))) : NIL ) );
}
static value *f13 (struct procedure *proc, value **arguments) {
return (value *) (arguments[0] ? (value *) arguments[0] : NIL );
}
static value *f14 (struct procedure *proc, value **arguments) {
return (value *) (arguments[0] ? (value *) arguments[0] : NIL );
}
static value *f15 (struct procedure *proc, value **arguments) {
return (value *) (arguments[0] ? (value *) call_procedure((procedure *) new_procedure(proc, arguments, f14, 1), (value *) call_procedure((procedure *) global_environment[2], (value *) rest((cell *) proc->environment[2]), (value *) rest((cell *) proc->environment[3]))) : NIL );
}
static value *f16 (struct procedure *proc, value **arguments) {
return (value *) (arguments[0] ? (value *) call_procedure((procedure *) new_procedure(proc, arguments, f13, 1), (value *) call_procedure((procedure *) new_procedure(proc, arguments, f15, 1), (value *) call_procedure((procedure *) global_environment[2], (value *) first((cell *) proc->environment[1]), (value *) first((cell *) proc->environment[2])))) : NIL );
}
static value *f17 (struct procedure *proc, value **arguments) {
return (value *) (arguments[0] ? (value *) call_procedure((procedure *) new_procedure(proc, arguments, f16, 1), (value *) cell_p((value *) proc->environment[1])) : NIL );
}
static value *f18 (struct procedure *proc, value **arguments) {
return (value *) call_procedure((procedure *) new_procedure(proc, arguments, f12, 1), (value *) call_procedure((procedure *) new_procedure(proc, arguments, f17, 1), (value *) cell_p((value *) arguments[0])));
}
static value *f19 (struct procedure *proc, value **arguments) {
return (value *) (arguments[2] ? (value *) call_procedure((procedure *) global_environment[3], (value *) proc->environment[0], (value *) new_cell((value *) call_procedure((procedure *) proc->environment[0], (value *) arguments[0], (value *) arguments[1]), (cell *) arguments[2])) : (global_environment[1] ? (value *) call_procedure((procedure *) proc->environment[0], (value *) arguments[0], (value *) arguments[1]) : NIL ) );
}
static value *f20 (struct procedure *proc, value **arguments) {
return (value *) call_procedure((procedure *) new_procedure(proc, arguments, f19, 3), (value *) first((cell *) arguments[1]), (value *) first((cell *) rest((cell *) arguments[1])), (value *) rest((cell *) rest((cell *) arguments[1])));
}
static value *f21 (struct procedure *proc, value **arguments) {
return (value *) (call_procedure((procedure *) proc->environment[0], (value *) arguments[0]) ? (value *) arguments[0] : (global_environment[1] ? (value *) (arguments[1] ? (value *) call_procedure((procedure *) global_environment[4], (value *) proc->environment[0], (value *) arguments[1]) : NIL ) : NIL ) );
}
static value *f22 (struct procedure *proc, value **arguments) {
return (value *) call_procedure((procedure *) new_procedure(proc, arguments, f21, 2), (value *) first((cell *) arguments[1]), (value *) rest((cell *) arguments[1]));
}
static value *f23 (struct procedure *proc, value **arguments) {
return (value *) first((cell *) arguments[0]);
}
static value *f24 (struct procedure *proc, value **arguments) {
return (value *) first((cell *) rest((cell *) arguments[0]));
}
static value *f25 (struct procedure *proc, value **arguments) {
return (value *) symbolic_equality((symbol *) arguments[0], (symbol *) proc->environment[0]);
}
static value *f26 (struct procedure *proc, value **arguments) {
return (value *) call_procedure((procedure *) global_environment[4], (value *) new_procedure(proc, arguments, f25, 1), (value *) arguments[1]);
}
static value *f27 (struct procedure *proc, value **arguments) {
return (value *) call_procedure((procedure *) global_environment[8], (value *) proc->environment[0], (value *) call_procedure((procedure *) global_environment[6], (value *) arguments[0]));
}
static value *f28 (struct procedure *proc, value **arguments) {
return (value *) call_procedure((procedure *) global_environment[4], (value *) new_procedure(proc, arguments, f27, 1), (value *) arguments[1]);
}
static value *f29 (struct procedure *proc, value **arguments) {
return (value *) first((cell *) arguments[0]);
}
static value *f30 (struct procedure *proc, value **arguments) {
return (value *) first((cell *) rest((cell *) arguments[0]));
}
static value *f31 (struct procedure *proc, value **arguments) {
return (value *) divide_number((number *) arguments[0], (number *) arguments[1]);
}
static value *f32 (struct procedure *proc, value **arguments) {
return (value *) multiply_number((number *) arguments[0], (number *) arguments[1]);
}
static value *f33 (struct procedure *proc, value **arguments) {
return (value *) call_procedure((procedure *) global_environment[3], (value *) new_procedure(proc, arguments, f31, 2), (value *) new_cell((value *) call_procedure((procedure *) global_environment[3], (value *) new_procedure(proc, arguments, f32, 2), (value *) new_cell((value *) call_procedure((procedure *) global_environment[7], (value *) arguments[1]), (cell *) new_cell((value *) arguments[0], (cell *) global_environment[0]))), (cell *) new_cell((value *) call_procedure((procedure *) global_environment[7], (value *) arguments[2]), (cell *) global_environment[0])));
}
static value *f34 (struct procedure *proc, value **arguments) {
return (value *) first((cell *) arguments[0]);
}
static value *f35 (struct procedure *proc, value **arguments) {
return (value *) rest((cell *) arguments[0]);
}
static value *f36 (struct procedure *proc, value **arguments) {
return (value *) (arguments[0] ? (value *) arguments[0] : NIL );
}
static value *f37 (struct procedure *proc, value **arguments) {
return (value *) (arguments[0] ? (value *) arguments[0] : NIL );
}
static value *f38 (struct procedure *proc, value **arguments) {
return (value *) (symbol_p((value *) arguments[0]) ? (value *) global_environment[0] : (global_environment[1] ? (value *) global_environment[1] : NIL ) );
}
static value *f39 (struct procedure *proc, value **arguments) {
return (value *) (arguments[0] ? (value *) call_procedure((procedure *) new_procedure(proc, arguments, f37, 1), (value *) (call_procedure((procedure *) global_environment[4], (value *) new_procedure(proc, arguments, f38, 1), (value *) proc->environment[3]) ? (value *) global_environment[0] : (global_environment[1] ? (value *) global_environment[1] : NIL ) )) : NIL );
}
static value *f40 (struct procedure *proc, value **arguments) {
return (value *) (arguments[0] ? (value *) call_procedure((procedure *) new_procedure(proc, arguments, f39, 1), (value *) cell_p((value *) proc->environment[2])) : NIL );
}
static value *f41 (struct procedure *proc, value **arguments) {
return (value *) (arguments[0] ? (value *) call_procedure((procedure *) new_procedure(proc, arguments, f40, 1), (value *) symbol_p((value *) call_procedure((procedure *) global_environment[11], (value *) proc->environment[0]))) : NIL );
}
static value *f42 (struct procedure *proc, value **arguments) {
return (value *) call_procedure((procedure *) new_procedure(proc, arguments, f41, 1), (value *) number_p((value *) call_procedure((procedure *) global_environment[10], (value *) arguments[0])));
}
static value *f43 (struct procedure *proc, value **arguments) {
return (value *) (arguments[0] ? (value *) call_procedure((procedure *) new_procedure(proc, arguments, f36, 1), (value *) call_procedure((procedure *) new_procedure(proc, arguments, f42, 2), (value *) call_procedure((procedure *) global_environment[13], (value *) proc->environment[0]), (value *) call_procedure((procedure *) global_environment[14], (value *) proc->environment[0]))) : NIL );
}
static value *f44 (struct procedure *proc, value **arguments) {
return (value *) call_procedure((procedure *) new_procedure(proc, arguments, f43, 1), (value *) cell_p((value *) arguments[0]));
}
static value *f45 (struct procedure *proc, value **arguments) {
return (value *) (arguments[0] ? (value *) arguments[0] : NIL );
}
static value *f46 (struct procedure *proc, value **arguments) {
return (value *) (arguments[0] ? (value *) call_procedure((procedure *) new_procedure(proc, arguments, f45, 1), (value *) proc->environment[0]) : NIL );
}
static value *f47 (struct procedure *proc, value **arguments) {
return (value *) (call_procedure((procedure *) new_procedure(proc, arguments, f46, 1), (value *) proc->environment[3]) ? (value *) new_cell((value *) call_procedure((procedure *) global_environment[12], (value *) proc->environment[2], (value *) proc->environment[3], (value *) arguments[0]), (cell *) new_cell((value *) proc->environment[0], (cell *) global_environment[0])) : (global_environment[1] ? (value *) new_symbol("UNKNOWN-UNIT-IDENTIFIER") : NIL ) );
}
static value *f48 (struct procedure *proc, value **arguments) {
return (value *) call_procedure((procedure *) new_procedure(proc, arguments, f47, 1), (value *) call_procedure((procedure *) global_environment[9], (value *) arguments[0], (value *) global_environment[5]));
}
static value *f49 (struct procedure *proc, value **arguments) {
return (value *) call_procedure((procedure *) arguments[0], (value *) new_procedure(proc, arguments, f48, 1), (value *) proc->environment[2]);
}
static value *f50 (struct procedure *proc, value **arguments) {
return (value *) call_procedure((procedure *) arguments[0], (value *) arguments[0]);
}
static value *f51 (struct procedure *proc, value **arguments) {
return (value *) call_procedure((procedure *) call_procedure((procedure *) proc->environment[0], (value *) proc->environment[0]), (value *) arguments[0], (value *) arguments[1]);
}
static value *f52 (struct procedure *proc, value **arguments) {
return (value *) call_procedure((procedure *) proc->environment[0], (value *) new_procedure(proc, arguments, f51, 2));
}
static value *f53 (struct procedure *proc, value **arguments) {
return (value *) call_procedure((procedure *) new_procedure(proc, arguments, f50, 1), (value *) new_procedure(proc, arguments, f52, 1));
}
static value *f54 (struct procedure *proc, value **arguments) {
return (value *) (arguments[0] ? (value *) arguments[0] : NIL );
}
static value *f55 (struct procedure *proc, value **arguments) {
return (value *) (call_procedure((procedure *) new_procedure(proc, arguments, f54, 1), (value *) arguments[1]) ? (value *) new_cell((value *) call_procedure((procedure *) arguments[0], (value *) first((cell *) arguments[1])), (cell *) call_procedure((procedure *) proc->environment[0], (value *) arguments[0], (value *) rest((cell *) arguments[1]))) : NIL );
}
static value *f56 (struct procedure *proc, value **arguments) {
return (value *) new_procedure(proc, arguments, f55, 2);
}
static value *f57 (struct procedure *proc, value **arguments) {
return (value *) call_procedure((procedure *) new_procedure(proc, arguments, f49, 1), (value *) call_procedure((procedure *) new_procedure(proc, arguments, f53, 1), (value *) new_procedure(proc, arguments, f56, 1)));
}
static value *f58 (struct procedure *proc, value **arguments) {
return (value *) call_procedure((procedure *) new_procedure(proc, arguments, f57, 3), (value *) call_procedure((procedure *) global_environment[10], (value *) call_procedure((procedure *) global_environment[13], (value *) arguments[0])), (value *) call_procedure((procedure *) global_environment[9], (value *) call_procedure((procedure *) global_environment[11], (value *) call_procedure((procedure *) global_environment[13], (value *) arguments[0])), (value *) global_environment[5]), (value *) call_procedure((procedure *) global_environment[14], (value *) arguments[0]));
}
static value *f59 (struct procedure *proc, value **arguments) {
return (value *) write((value *) (call_procedure((procedure *) global_environment[15], (value *) arguments[0]) ? (value *) call_procedure((procedure *) global_environment[16], (value *) arguments[0]) : (global_environment[1] ? (value *) new_symbol("INVALID-QUERY") : NIL ) ), (cell *) global_environment[0]);
}
void fmain (void) {
free_value((value *) call_procedure((procedure *) new_procedure(proc, arguments, f59, 1), (value *) read((cell *) global_environment[0])));
}
int main (void) {
use(global_environment[0] = (value *) NIL);
use(global_environment[1] = (value *) new_symbol("T"));
use(global_environment[2] = (value *) new_procedure(proc, arguments, f18, 2));
use(global_environment[3] = (value *) new_procedure(proc, arguments, f20, 2));
use(global_environment[4] = (value *) new_procedure(proc, arguments, f22, 2));
use(global_environment[5] = (value *) new_cell((value *) new_cell((value *) new_cell((value *) new_symbol("MILLIMETER"), new_cell((value *) new_symbol("MM"), NIL)), new_cell((value *) new_number(1, 1), NIL)), new_cell((value *) new_cell((value *) new_cell((value *) new_symbol("CENTIMETER"), new_cell((value *) new_symbol("CM"), NIL)), new_cell((value *) new_number(10, 1), NIL)), new_cell((value *) new_cell((value *) new_cell((value *) new_symbol("METER"), new_cell((value *) new_symbol("M"), NIL)), new_cell((value *) new_number(1000, 1), NIL)), new_cell((value *) new_cell((value *) new_cell((value *) new_symbol("INCH"), new_cell((value *) new_symbol("IN"), new_cell((value *) new_symbol("ZOLL"), NIL))), new_cell((value *) new_number(127, 5), NIL)), new_cell((value *) new_cell((value *) new_cell((value *) new_symbol("FEET"), new_cell((value *) new_symbol("FT"), new_cell((value *) new_symbol("FUß"), NIL))), new_cell((value *) new_number(1524, 5), NIL)), NIL))))));
use(global_environment[6] = (value *) new_procedure(proc, arguments, f23, 1));
use(global_environment[7] = (value *) new_procedure(proc, arguments, f24, 1));
use(global_environment[8] = (value *) new_procedure(proc, arguments, f26, 2));
use(global_environment[9] = (value *) new_procedure(proc, arguments, f28, 2));
use(global_environment[10] = (value *) new_procedure(proc, arguments, f29, 1));
use(global_environment[11] = (value *) new_procedure(proc, arguments, f30, 1));
use(global_environment[12] = (value *) new_procedure(proc, arguments, f33, 3));
use(global_environment[13] = (value *) new_procedure(proc, arguments, f34, 1));
use(global_environment[14] = (value *) new_procedure(proc, arguments, f35, 1));
use(global_environment[15] = (value *) new_procedure(proc, arguments, f44, 1));
use(global_environment[16] = (value *) new_procedure(proc, arguments, f58, 1));
fmain();
disuse(global_environment[0]);
disuse(global_environment[1]);
disuse(global_environment[2]);
disuse(global_environment[3]);
disuse(global_environment[4]);
disuse(global_environment[5]);
disuse(global_environment[6]);
disuse(global_environment[7]);
disuse(global_environment[8]);
disuse(global_environment[9]);
disuse(global_environment[10]);
disuse(global_environment[11]);
disuse(global_environment[12]);
disuse(global_environment[13]);
disuse(global_environment[14]);
disuse(global_environment[15]);
disuse(global_environment[16]);
collect_garbage();
return 0;
}
