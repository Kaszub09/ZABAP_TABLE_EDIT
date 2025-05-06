CLASS tcl_zabap_table_fields DEFINITION FINAL FOR TESTING RISK LEVEL HARMLESS DURATION SHORT.

  PRIVATE SECTION.
    METHODS:
      setup,
      mandant_detection              FOR TESTING,
      key_fields_only_detection      FOR TESTING,
      get_field_catalogue            FOR TESTING,
      edit_mode                      FOR TESTING,
      key_structure_has_key_fields   FOR TESTING,
      key_structure_has_index_field  FOR TESTING,
      key_table_is_of_line_struct    FOR TESTING,
      key_table_if_is_of_line_struct FOR TESTING,
      get_fc_with_add_fields         FOR TESTING,
      base_structure                 FOR TESTING,
      base_structure_with_add_fields FOR TESTING,
      base_table                     FOR TESTING,
      base_table_with_add_fields     FOR TESTING,
      non_key_structure              FOR TESTING,
      non_key_tab_is_of_struct_line  FOR TESTING,
      validate_field_catalogue,
      check_tab_is_of_line_struct IMPORTING struct TYPE REF TO cl_abap_structdescr table TYPE REF TO cl_abap_tabledescr.

    CLASS-DATA:
           struct_name TYPE string VALUE 'ZABAP_TE_CP_TEST'.

    DATA:
      cut TYPE REF TO zcl_zabap_table_fields,
      fc  TYPE zcl_zabap_field_catalogue=>tt_field_cat.
ENDCLASS.

CLASS tcl_zabap_table_fields IMPLEMENTATION.
  METHOD setup.
    cut = NEW #( struct_name ).
  ENDMETHOD.

  METHOD key_fields_only_detection.
    cut = NEW #( 'ZABAP_TE_KO_TEST' ).
    cl_abap_unit_assert=>assert_true( cut->key_fields_only ).
  ENDMETHOD.

  METHOD mandant_detection.
    "No mandant
    cut = NEW #( 'ZABAP_TE_KO_TEST' ).
    cl_abap_unit_assert=>assert_false( cut->has_mandant ).

    "With mandant
    cut = NEW #( 'ZABAP_TE_CP_TEST' ).
    cl_abap_unit_assert=>assert_true( cut->has_mandant ).
    cl_abap_unit_assert=>assert_equals( exp = 'MANDT' act = cut->mandant_field ).
  ENDMETHOD.

  METHOD get_field_catalogue.
    fc = cut->get_field_catalogue( ).
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

  METHOD edit_mode.
    cut->is_in_edit_mode = abap_true.
    fc = cut->get_field_catalogue( ).
    cl_abap_unit_assert=>assert_true( fc[ KEY name fieldname = 'MANDT' ]-edit ).
    cl_abap_unit_assert=>assert_true( fc[ KEY name fieldname = 'KEY1' ]-edit ).
    cl_abap_unit_assert=>assert_true( fc[ KEY name fieldname = 'VAL2' ]-edit ).

    cut->is_in_edit_mode = abap_false.
    fc = cut->get_field_catalogue( ).
    cl_abap_unit_assert=>assert_false( fc[ KEY name fieldname = 'MANDT' ]-edit ).
    cl_abap_unit_assert=>assert_false( fc[ KEY name fieldname = 'KEY1' ]-edit ).
    cl_abap_unit_assert=>assert_false( fc[ KEY name fieldname = 'VAL2' ]-edit ).
  ENDMETHOD.

  METHOD key_structure_has_key_fields.
    cut->get_keys_structure( EXPORTING include_index_field = abap_false IMPORTING struct = DATA(struct) ).
    cl_abap_unit_assert=>assert_equals( exp = 4 act = lines( struct->components ) ).

    create_data created_struct struct.
    ASSIGN created_struct->* TO FIELD-SYMBOL(<created_struct>).

    ASSIGN COMPONENT 'MANDT' OF STRUCTURE <created_struct> TO FIELD-SYMBOL(<mandt_field>).
    cl_abap_unit_assert=>assert_subrc( ).
    ASSIGN COMPONENT 'KEY1' OF STRUCTURE <created_struct> TO FIELD-SYMBOL(<key1_field>).
    cl_abap_unit_assert=>assert_subrc( ).
    ASSIGN COMPONENT 'KEY2' OF STRUCTURE <created_struct> TO FIELD-SYMBOL(<key2_field>).
    cl_abap_unit_assert=>assert_subrc( ).
    ASSIGN COMPONENT 'KEY3' OF STRUCTURE <created_struct> TO FIELD-SYMBOL(<key3_field>).
    cl_abap_unit_assert=>assert_subrc( ).
  ENDMETHOD.

  METHOD key_structure_has_index_field.
    cut->get_keys_structure( EXPORTING include_index_field = abap_true IMPORTING struct = DATA(struct) index_field_name = DATA(index_field) ).
    cl_abap_unit_assert=>assert_equals( exp = 5 act = lines( struct->components ) ).

    create_data created_struct struct.
    ASSIGN created_struct->* TO FIELD-SYMBOL(<created_struct>).

    ASSIGN COMPONENT index_field OF STRUCTURE <created_struct> TO FIELD-SYMBOL(<index_field>).
    cl_abap_unit_assert=>assert_subrc( ).
  ENDMETHOD.

  METHOD key_table_is_of_line_struct.
    cut->get_keys_structure( EXPORTING include_index_field = abap_false IMPORTING struct = DATA(struct) table = DATA(table) ).
    check_tab_is_of_line_struct( struct = struct table = table ).
  ENDMETHOD.

  METHOD key_table_if_is_of_line_struct.
    cut->get_keys_structure( EXPORTING include_index_field = abap_true IMPORTING struct = DATA(struct) table = DATA(table) ).
    check_tab_is_of_line_struct( struct = struct table = table ).
  ENDMETHOD.

  METHOD get_fc_with_add_fields.
    DATA(add_fields) = VALUE cl_abap_structdescr=>component_table( ( name = 'ADD'
        type = CAST #( cl_abap_elemdescr=>describe_by_name( 'ZABAP_TABLE_EDIT_CD_NO_TRACK' ) ) ) ).
    DATA(fc) = cut->get_fc_with_add_fields( add_fields ).

    cl_abap_unit_assert=>assert_true( act = xsdbool( line_exists( fc[ KEY name fieldname = 'MANDT' ] ) ) ).
    cl_abap_unit_assert=>assert_true( act = xsdbool( line_exists( fc[ KEY name fieldname = 'ADD' ] ) ) ).
    cl_abap_unit_assert=>assert_equals( exp = 7 act = lines( fc ) ).
  ENDMETHOD.

  METHOD base_structure.
    DATA(add_fields) = VALUE cl_abap_structdescr=>component_table( ).
    cut->get_base_with_add_fields( EXPORTING additional_fields = add_fields IMPORTING struct = DATA(struct) ).

    create_data created_struct struct.

    FIELD-SYMBOLS: <original_struct> TYPE zabap_te_cp_test.
    ASSIGN created_struct->* TO <original_struct>.
    cl_abap_unit_assert=>assert_subrc( ).
  ENDMETHOD.

  METHOD base_table.
    DATA(add_fields) = VALUE cl_abap_structdescr=>component_table( ).
    cut->get_base_with_add_fields( EXPORTING additional_fields = add_fields IMPORTING table = DATA(table) ).

    create_data created_table table.

    TYPES: tt_zabap_te_cp_test TYPE STANDARD TABLE OF zabap_te_cp_test WITH EMPTY KEY.
    FIELD-SYMBOLS: <original_table> TYPE tt_zabap_te_cp_test.
    ASSIGN created_table->* TO <original_table>.
    cl_abap_unit_assert=>assert_subrc( ).
  ENDMETHOD.

  METHOD base_structure_with_add_fields.
    DATA(add_fields) = VALUE cl_abap_structdescr=>component_table( ( name = 'ADD'
           type = CAST #( cl_abap_elemdescr=>describe_by_name( 'ZABAP_TABLE_EDIT_CD_NO_TRACK' ) ) ) ).
    cut->get_base_with_add_fields( EXPORTING additional_fields = add_fields IMPORTING struct = DATA(struct) table = DATA(table) ).

    cl_abap_unit_assert=>assert_equals( exp = 7 act = lines( struct->components ) ).

    create_data created_struct struct.
    ASSIGN created_struct->* TO FIELD-SYMBOL(<created_struct>).

    ASSIGN COMPONENT 'ADD' OF STRUCTURE <created_struct> TO FIELD-SYMBOL(<index_field>).
    cl_abap_unit_assert=>assert_subrc( ).
    ASSIGN COMPONENT 'KEY1' OF STRUCTURE <created_struct> TO FIELD-SYMBOL(<key1_field>).
    cl_abap_unit_assert=>assert_subrc( ).
  ENDMETHOD.

  METHOD base_table_with_add_fields.
    DATA(add_fields) = VALUE cl_abap_structdescr=>component_table( ( name = 'ADD'
           type = CAST #( cl_abap_elemdescr=>describe_by_name( 'ZABAP_TABLE_EDIT_CD_NO_TRACK' ) ) ) ).
    cut->get_base_with_add_fields( EXPORTING additional_fields = add_fields IMPORTING struct = DATA(struct) table = DATA(table) ).
    check_tab_is_of_line_struct( struct = struct table = table ).
  ENDMETHOD.

  METHOD non_key_structure.
    cut->get_non_keys_structure( IMPORTING struct = DATA(struct) ).
    cl_abap_unit_assert=>assert_equals( exp = 2 act = lines( struct->components ) ).

    create_data created_struct struct.
    ASSIGN created_struct->* TO FIELD-SYMBOL(<created_struct>).

    ASSIGN COMPONENT 'VAL1' OF STRUCTURE <created_struct> TO FIELD-SYMBOL(<val1_field>).
    cl_abap_unit_assert=>assert_subrc( ).
    ASSIGN COMPONENT 'VAL2' OF STRUCTURE <created_struct> TO FIELD-SYMBOL(<val2_field>).
    cl_abap_unit_assert=>assert_subrc( ).
  ENDMETHOD.

  METHOD non_key_tab_is_of_struct_line.
    cut->get_non_keys_structure( IMPORTING struct = DATA(struct) table = DATA(table) ).
    check_tab_is_of_line_struct( struct = struct table = table ).
  ENDMETHOD.

  METHOD check_tab_is_of_line_struct.
    create_data created_struct struct.
    ASSIGN created_struct->* TO FIELD-SYMBOL(<created_struct>).

    create_data created_table table.
    assign_to_table_fs created_table->* <table>.

    APPEND <created_struct> TO <table>.
    cl_abap_unit_assert=>assert_equals( exp = 1 act = lines( <table> ) ).
  ENDMETHOD.
ENDCLASS.
