CLASS ltcl DEFINITION DEFERRED.
CLASS zcl_zabap_table_edit_txt_tab_e DEFINITION LOCAL FRIENDS ltcl.
CLASS ltcl DEFINITION FINAL FOR TESTING RISK LEVEL HARMLESS DURATION SHORT.

  PRIVATE SECTION.
    METHODS:
      setup,
      lock_always_sucessfull FOR TESTING RAISING cx_static_check.

    DATA:
        cut TYPE REF TO zcl_zabap_table_edit_txt_tab_e.
ENDCLASS.

CLASS ltcl IMPLEMENTATION.
  METHOD setup.
    cut = NEW #( ).
  ENDMETHOD.

  METHOD lock_always_sucessfull.
    cl_abap_unit_assert=>assert_true( cut->zif_zabap_table_edit_text_tab~lock_table( ) ).
  ENDMETHOD.
ENDCLASS.
