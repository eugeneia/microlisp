/* error.h:  Enumeration of error conditions. */

enum error {
  OUT_OF_MEMORY          = 1,
  OUT_OF_SYMBOLS         = 2,
  INVALID_TYPE           = 3,
  INVALID_SYMBOL_NAME    = 4,
  REFERENCE_OVERFLOW     = 5,
  INVALID_IO_RESOURCE    = 6,
  INVALID_IO_UNPRINTABLE = 7,
  RUNTIME_CORRUPTION     = 8
};
