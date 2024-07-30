CLASS zcl_zabap_table_edit_globals DEFINITION PUBLIC CREATE PRIVATE.

  PUBLIC SECTION.
    TYPES:
      BEGIN OF t_data_comparision,
        duplicates       TYPE REF TO data,
        inserted         TYPE REF TO data,
        deleted          TYPE REF TO data,
        before_modified  TYPE REF TO data,
        modified         TYPE REF TO data,
        not_in_selection TYPE REF TO data,
      END OF t_data_comparision.

    CONSTANTS:
      BEGIN OF c_validation,
        incorrect_values  TYPE i VALUE 0,
        duplicates        TYPE i VALUE 1,
        ok                TYPE i VALUE 2,
        extension_invalid TYPE i VALUE 3,
        not_in_selection  TYPE i VALUE 4,
      END OF c_validation.
ENDCLASS.

CLASS zcl_zabap_table_edit_globals IMPLEMENTATION.
ENDCLASS.
