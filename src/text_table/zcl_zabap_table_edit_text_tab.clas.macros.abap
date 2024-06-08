*"* use this source file for any macro definitions you need
*"* in the implementation part of the class
DEFINE assign_to_table_fs.
  FIELD-SYMBOLS &2 TYPE table.
  ASSIGN &1 TO &2.
end-of-definition.

DEFINE assign_to_any_fs.
  FIELD-SYMBOLS &2 TYPE any.
  ASSIGN &1 TO &2.
end-of-definition.


DEFINE create_and_assign_data.
    DATA &1 TYPE REF TO data.
    CREATE DATA &1 TYPE HANDLE &2.
    FIELD-SYMBOLS &3 TYPE any.
    ASSIGN  &1->* TO &3.
end-of-definition.
