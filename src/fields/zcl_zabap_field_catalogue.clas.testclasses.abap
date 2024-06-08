CLASS tcl_zabap_field_catalogue DEFINITION FINAL FOR TESTING RISK LEVEL HARMLESS DURATION SHORT.

  PRIVATE SECTION.
    METHODS:
      setup,
      validate_from_struct      FOR TESTING,
      validate_from_structdescr FOR TESTING,
      validate_from_name        FOR TESTING,
      validate_checkbox         FOR TESTING,
      validate_field_catalogue.

    DATA:
        struct_name TYPE string VALUE 'ZABAP_TE_CP_TEST'.
    DATA:
      cut TYPE REF TO zcl_zabap_field_catalogue,
      fc  TYPE zcl_zabap_field_catalogue=>tt_field_cat.
ENDCLASS.

CLASS tcl_zabap_field_catalogue IMPLEMENTATION.
  METHOD setup.
    cut = NEW #( ).
  ENDMETHOD.

  METHOD validate_from_name.
    fc = cut->get_fc_from_struct_name( struct_name ).
    validate_field_catalogue( ).
  ENDMETHOD.

  METHOD validate_from_struct.
    DATA(structure) = VALUE zabap_te_cp_test( ).
    fc = cut->get_fc_from_struct( structure ).
    validate_field_catalogue( ).
  ENDMETHOD.

  METHOD validate_from_structdescr.
    fc = cut->get_fc_from_structdescr( CAST #( cl_abap_structdescr=>describe_by_name( struct_name ) ) ).
    validate_field_catalogue( ).
  ENDMETHOD.

  METHOD validate_field_catalogue.
    "Fields names and count
    cl_abap_unit_assert=>assert_true( act = xsdbool( line_exists( fc[ KEY name fieldname = 'MANDT' ] ) ) ).
    cl_abap_unit_assert=>assert_true( act = xsdbool( line_exists( fc[ KEY name fieldname = 'KEY1' ] ) ) ).
    cl_abap_unit_assert=>assert_true( act = xsdbool( line_exists( fc[ KEY name fieldname = 'KEY2' ] ) ) ).
    cl_abap_unit_assert=>assert_true( act = xsdbool( line_exists( fc[ KEY name fieldname = 'KEY3' ] ) ) ).
    cl_abap_unit_assert=>assert_true( act = xsdbool( line_exists( fc[ KEY name fieldname = 'VAL1' ] ) ) ).
    cl_abap_unit_assert=>assert_true( act = xsdbool( line_exists( fc[ KEY name fieldname = 'VAL2' ] ) ) ).
    cl_abap_unit_assert=>assert_false( act = xsdbool( line_exists( fc[ KEY name fieldname = 'I_DONT_EXIST' ] ) ) ).
    cl_abap_unit_assert=>assert_equals( exp = 6 act = lines( fc ) ).
    "Key/non-key fields
    cl_abap_unit_assert=>assert_true( act = fc[ KEY name fieldname = 'MANDT' ]-key ).
    cl_abap_unit_assert=>assert_true( act = fc[ KEY name fieldname = 'KEY1' ]-key ).
    cl_abap_unit_assert=>assert_true( act = fc[ KEY name fieldname = 'KEY2' ]-key ).
    cl_abap_unit_assert=>assert_true( act = fc[ KEY name fieldname = 'KEY3' ]-key ).
    cl_abap_unit_assert=>assert_false( act = fc[ KEY name fieldname = 'VAL1' ]-key ).
    cl_abap_unit_assert=>assert_false( act = fc[ KEY name fieldname = 'VAL2' ]-key ).
    "Types
    cl_abap_unit_assert=>assert_equals( exp = 'MANDT' act = fc[ KEY name fieldname = 'MANDT' ]-dd_roll ).
    cl_abap_unit_assert=>assert_equals( exp = 'ZABAP_TABLE_EDIT_CD_NO_TRACK' act = fc[ KEY name fieldname = 'KEY1' ]-dd_roll ).
    cl_abap_unit_assert=>assert_equals( exp = 'ZABAP_TABLE_EDIT_CD_SIGN_DEC' act = fc[ KEY name fieldname = 'KEY2' ]-dd_roll ).
    cl_abap_unit_assert=>assert_equals( exp = 'ZABAP_TABLE_EDIT_CD_TRACK' act = fc[ KEY name fieldname = 'KEY3' ]-dd_roll ).
    cl_abap_unit_assert=>assert_equals( exp = 'ZABAP_TABLE_EDIT_CD_NO_TRACK' act = fc[ KEY name fieldname = 'VAL1' ]-dd_roll ).
    cl_abap_unit_assert=>assert_equals( exp = 'ZABAP_TABLE_EDIT_CD_TRACK' act = fc[ KEY name fieldname = 'VAL2' ]-dd_roll ).
  ENDMETHOD.

  METHOD validate_checkbox.
    fc = cut->get_fc_from_struct_name( 'ZABAP_TE_FC_CHECKBOX' ).

    cl_abap_unit_assert=>assert_true( act = xsdbool( line_exists( fc[ KEY name fieldname = 'CHECKBOX' ] ) ) ).
    cl_abap_unit_assert=>assert_true( act = fc[ KEY name fieldname = 'CHECKBOX' ]-checkbox ).
  ENDMETHOD.
ENDCLASS.
