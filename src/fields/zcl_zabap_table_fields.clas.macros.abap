*"* use this source file for any macro definitions you need
*"* in the implementation part of the class
DEFINE create_data.
  DATA &1 TYPE REF TO data.
  CREATE DATA &1 TYPE HANDLE &2.
END-OF-DEFINITION.

DEFINE assign_to_table_fs.
  FIELD-SYMBOLS: &2 TYPE table.
  ASSIGN &1 TO &2.
END-OF-DEFINITION.

DEFINE assign_to_any_fs.
  FIELD-SYMBOLS: &2 TYPE any.
  ASSIGN &1 TO &2.
END-OF-DEFINITION.
