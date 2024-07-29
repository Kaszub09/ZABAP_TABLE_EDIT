CLASS zcl_zabap_table_edit_restr_sel DEFINITION PUBLIC FINAL CREATE PUBLIC.

  PUBLIC SECTION.
    METHODS:
      constructor IMPORTING table_name TYPE string,
      display.

  PRIVATE SECTION.
    DATA:
      table_name TYPE string.
ENDCLASS.

CLASS zcl_zabap_table_edit_restr_sel IMPLEMENTATION.
  METHOD constructor.
    me->table_name = table_name.
  ENDMETHOD.
  METHOD display.

  ENDMETHOD.
ENDCLASS.
