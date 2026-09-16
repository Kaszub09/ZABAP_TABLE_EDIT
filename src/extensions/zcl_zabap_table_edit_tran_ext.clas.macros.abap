*"* use this source file for any macro definitions you need
*"* in the implementation part of the class
DEFINE get_row.
  FIELD-SYMBOLS: <table> TYPE STANDARD TABLE.
  ASSIGN modified_data_ext->* TO <table>.
  ASSIGN <table>[ &1 ] TO FIELD-SYMBOL(&2).
end-of-DEFINITION.
