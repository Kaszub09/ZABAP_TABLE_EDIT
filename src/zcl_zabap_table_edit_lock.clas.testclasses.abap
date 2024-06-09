CONSTANTS:
  c_table TYPE string VALUE 'ZABAP_TE_TEST'.
CLASS tcl_zabap_table_edit_lock DEFINITION FINAL FOR TESTING RISK LEVEL HARMLESS DURATION SHORT.

  PRIVATE SECTION.
    METHODS:
      setup,
      lock_test_table FOR TESTING.

    DATA:
        cut TYPE REF TO zcl_zabap_table_edit_lock.
ENDCLASS.

CLASS tcl_zabap_table_edit_lock IMPLEMENTATION.
  METHOD setup.
    cut = NEW #( c_table ).
  ENDMETHOD.

  METHOD lock_test_table.
    cl_abap_unit_assert=>assert_true( cut->lock_table( ) ).
    cut->unlock_table( ).
  ENDMETHOD.
ENDCLASS.
