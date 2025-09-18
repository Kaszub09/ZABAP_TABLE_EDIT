TYPES:
  tt_zabap_te_td_test TYPE STANDARD TABLE OF zabap_te_td_test WITH EMPTY KEY.
CONSTANTS:
  c_field TYPE fieldname  VALUE 'FIELD',
  c_table TYPE string VALUE 'ZABAP_TE_TD_TEST'.
"=================================================================
CLASS lcl_test_data DEFINITION CREATE PUBLIC FOR TESTING.
  PUBLIC SECTION.
    INTERFACES:
      zif_zabap_table_edit_db.

    METHODS:
      get_data EXPORTING initial  TYPE REF TO data extended  TYPE REF TO data.

    DATA:
      initial_tab  TYPE tt_zabap_te_td_test,
      extended_tab TYPE tt_zabap_te_td_test.
ENDCLASS.

CLASS lcl_test_data IMPLEMENTATION.
  METHOD get_data.
    initial_tab = VALUE tt_zabap_te_td_test( mandt = sy-mandt
        ( key1 = 'DEL' ) ( key1 = 'MD1' val1 = 'V1' ) ( key1 = 'MD2' val2 = '' ) ).
    extended_tab = VALUE tt_zabap_te_td_test( mandt = sy-mandt
        ( key1 = 'INS' ) ( key1 = 'MD1' val1 = '' ) ( key1 = 'MD2' val2 = 'VAL2' ) ).

    initial = REF #( initial_tab ).
    extended = REF #( extended_tab ).
  ENDMETHOD.

  METHOD zif_zabap_table_edit_db~delete_data.
    cl_abap_unit_assert=>assert_equals( exp = c_table act = table ).
    DATA(expected) = VALUE tt_zabap_te_td_test( mandt = sy-mandt ( key1 = 'DEL' ) ).
    cl_abap_unit_assert=>assert_equals( exp = expected act = table_data ).
  ENDMETHOD.

  METHOD zif_zabap_table_edit_db~delete_data_where.
    cl_abap_unit_assert=>fail( |Deletion should be via delete| ).
  ENDMETHOD.

  METHOD zif_zabap_table_edit_db~insert_data.
    cl_abap_unit_assert=>assert_equals( exp = c_table act = table ).
  ENDMETHOD.

  METHOD zif_zabap_table_edit_db~modify_data.
    cl_abap_unit_assert=>assert_equals( exp = c_table act = table ).
    IF lines( table_data ) = 1.
      DATA(expected) = VALUE tt_zabap_te_td_test( mandt = sy-mandt ( key1 = 'INS' ) ).
      cl_abap_unit_assert=>assert_equals( exp = expected act = table_data ).
    ELSEIF lines( table_data ) = 2.
      DATA(expected2) = VALUE tt_zabap_te_td_test( mandt = sy-mandt ( key1 = 'MD1' val1 = '' ) ( key1 = 'MD2' val2 = 'VAL2' ) ).
      cl_abap_unit_assert=>assert_equals( exp = expected2 act = table_data ).
    ELSE.
      cl_abap_unit_assert=>fail( |Unexpected number of rows| ).
    ENDIF.
  ENDMETHOD.
ENDCLASS.
"=================================================================
CLASS tcl_zabap_table_edit_tab_data DEFINITION DEFERRED.
CLASS zcl_zabap_table_edit_tab_data DEFINITION LOCAL FRIENDS tcl_zabap_table_edit_tab_data.
CLASS tcl_zabap_table_edit_tab_data DEFINITION FINAL FOR TESTING RISK LEVEL HARMLESS DURATION SHORT.

  PRIVATE SECTION.
    CLASS-METHODS:
      class_setup,
      class_teardown.

    METHODS:
      setup,
      can_lock                     FOR TESTING,
      get_selected_row_key         FOR TESTING,
      display_correct_data_on_grid FOR TESTING,
      valid_data                   FOR TESTING,
      invalid_data_duplicates      FOR TESTING,
      invalid_data_check_table     FOR TESTING,
      configure_db_validation IMPORTING pass TYPE abap_bool,
      correct_saved                FOR TESTING,
      cd_called_correctly          FOR TESTING,
      empty_rows_removed           FOR TESTING,
      restrict_selection_called FOR TESTING,
      restrict_selection_not_called FOR TESTING,
      selection_restricted FOR TESTING,
      invalid_data_restrict_sel FOR TESTING.

    CLASS-DATA:
       mocked_db TYPE tt_zabap_te_td_test.

    DATA:
      grid   TYPE REF TO zif_zabap_table_edit_grid_if,
      config TYPE zif_zabap_table_edit_tab_data=>t_config,
      cut    TYPE REF TO zif_zabap_table_edit_tab_data.
ENDCLASS.

CLASS tcl_zabap_table_edit_tab_data IMPLEMENTATION.
  METHOD class_setup.
    mocked_db = VALUE #( mandt = sy-mandt  ( key1 = 'KEY' ) ( key1 = '   AAA' ) ).
    MODIFY zabap_te_td_test FROM TABLE @mocked_db.
  ENDMETHOD.

  METHOD class_teardown.
    ROLLBACK WORK.
  ENDMETHOD.

  METHOD setup.
    DATA(empty) = NEW zcl_zabap_table_edit_empty_ext( ).
    config = VALUE #( table_name = c_table ext-commands = empty ext-data = empty ext-config = empty ).

    zcl_zabap_table_edit_fact_inj=>clear_all_injections( ).

    grid ?= cl_abap_testdouble=>create( 'ZIF_ZABAP_TABLE_EDIT_GRID_IF' ).
    cut = zcl_zabap_table_edit_factory=>get_table_data( config = config grid = grid ).
  ENDMETHOD.

  METHOD can_lock.
    cl_abap_unit_assert=>assert_true( cut->lock_table( ) ).
    cut->unlock_table( ).
  ENDMETHOD.

  METHOD get_selected_row_key.
    "Return first row key
    grid ?= cl_abap_testdouble=>create( 'ZIF_ZABAP_TABLE_EDIT_GRID_IF' ).
    cl_abap_testdouble=>configure_call( grid )->set_parameter( name = 'ET_INDEX_ROWS' value = VALUE lvc_t_row( ( index = 1 ) ) ).
    grid->get_selected_rows( ).

    cut = zcl_zabap_table_edit_factory=>get_table_data( config = config grid = grid ).
    cut->reset_grid( abap_false ).

    "Validate - AAA should be first in table since it's sorted by primary key
    cl_abap_unit_assert=>assert_equals( exp = |010   AAA| act =  cut->get_selected_row_key( ) ).
  ENDMETHOD.

  METHOD display_correct_data_on_grid.
    grid ?= cl_abap_testdouble=>create( 'ZIF_ZABAP_TABLE_EDIT_GRID_IF' ).

    "Expect data as in db
    cl_abap_testdouble=>configure_call( grid )->ignore_all_parameters(
        )->set_parameter( name = 'IT_OUTTAB' value = mocked_db
        )->and_expect( )->is_called_once( ).
    grid->set_table_for_first_display( CHANGING it_outtab = mocked_db ).

    cut = zcl_zabap_table_edit_factory=>get_table_data( config = config grid = grid ).
    cut->reset_grid( abap_false ).

    cl_abap_testdouble=>verify_expectations( grid ).
  ENDMETHOD.

  METHOD invalid_data_check_table.
    configure_db_validation( abap_false ).

    cut = zcl_zabap_table_edit_factory=>get_table_data( config = config grid = grid ).

    cut->validate( IMPORTING result = DATA(result) ).

    cl_abap_unit_assert=>assert_equals( exp = zif_zabap_table_edit_data=>c_validation-incorrect_values act = result ).
  ENDMETHOD.

  METHOD invalid_data_duplicates.
    configure_db_validation( abap_true ).

    "Duplicates
    DATA(modified) = VALUE tt_zabap_te_td_test( BASE mocked_db ( mocked_db[ 1 ] ) ).

    DATA(cut_base) = CAST zcl_zabap_table_edit_tab_data( cut ).
    cut_base->table-modified_data_ext = REF #( modified ).
    cut->validate( IMPORTING result = DATA(result) ).

    cl_abap_unit_assert=>assert_equals( exp = zif_zabap_table_edit_data=>c_validation-duplicates act = result ).
  ENDMETHOD.

  METHOD valid_data.
    configure_db_validation( abap_true ).

    "New row, ok data
    DATA(modified) = VALUE tt_zabap_te_td_test( BASE mocked_db ( key1 = 'XYZ' ) ).

    DATA(cut_base) = CAST zcl_zabap_table_edit_tab_data( cut ).
    cut_base->table-modified_data_ext = REF #( modified ).
    cut->validate( IMPORTING result = DATA(result) ).

    cl_abap_unit_assert=>assert_equals( exp = zif_zabap_table_edit_data=>c_validation-ok act = result ).
  ENDMETHOD.

  METHOD configure_db_validation.
    grid ?= cl_abap_testdouble=>create( 'ZIF_ZABAP_TABLE_EDIT_GRID_IF' ).
    cl_abap_testdouble=>configure_call( grid )->ignore_all_parameters(
        )->set_parameter( name = 'e_valid' value = pass ).
    grid->check_changed_data( ).

    cut = zcl_zabap_table_edit_factory=>get_table_data( config = config grid = grid ).
  ENDMETHOD.

  METHOD cd_called_correctly.
    DATA cd_states TYPE STANDARD TABLE OF c WITH EMPTY KEY.

    cd_states = VALUE #( ( space ) ( 'F' ) ( 'X' ) ).
    LOOP AT cd_states REFERENCE INTO DATA(cd_state).
      zcl_zabap_table_edit_fact_inj=>inject_cd( NEW zcl_zabap_table_edit_cd_test( cd_state->* ) ).

      correct_saved( ).
    ENDLOOP.
  ENDMETHOD.

  METHOD correct_saved.
    DATA(test_data) = NEW lcl_test_data( ).
    zcl_zabap_table_edit_fact_inj=>inject_db( test_data ).
    configure_db_validation( abap_true ).

    DATA(cut_base) = CAST zcl_zabap_table_edit_tab_data( cut ).
    test_data->get_data( IMPORTING initial = cut_base->table-initial_data extended = cut_base->table-modified_data_ext ).

    cut->validate( IMPORTING compared = DATA(compared) ).
    cl_abap_unit_assert=>assert_true( cut->save_data( CHANGING compared = compared ) ).
  ENDMETHOD.

  METHOD empty_rows_removed.
    configure_db_validation( abap_true ).

    DATA(modified) = VALUE tt_zabap_te_td_test( BASE mocked_db ( ) ( key1 = 'XYZ' ) ( ) ( ) ).

    DATA(cut_base) = CAST zcl_zabap_table_edit_tab_data( cut ).
    cut_base->table-modified_data_ext = REF #( modified ).
    cut->validate( ).

    cl_abap_unit_assert=>assert_equals( exp = VALUE tt_zabap_te_td_test( BASE mocked_db ( mandt = sy-mandt key1 = 'XYZ' ) ) act = modified ).
  ENDMETHOD.
  METHOD restrict_selection_called.
    DATA(selection) = CAST zif_zabap_table_edit_restr_sel( cl_abap_testdouble=>create( 'ZIF_ZABAP_TABLE_EDIT_RESTR_SEL' ) ).
    cl_abap_testdouble=>create( 'ZIF_ZABAP_TABLE_EDIT_RESTR_SEL' ).
    cl_abap_testdouble=>configure_call( selection )->ignore_all_parameters( )->and_expect( )->is_called_once( ).
    selection->display( abap_false ).

    config-show_selection_first = abap_true.
    zcl_zabap_table_edit_fact_inj=>inject_restrict_selection( selection ).

    cut = zcl_zabap_table_edit_factory=>get_table_data( config = config grid = grid ).

    cl_abap_testdouble=>verify_expectations( selection ).
  ENDMETHOD.

  METHOD restrict_selection_not_called.
    DATA(selection) = CAST zif_zabap_table_edit_restr_sel( cl_abap_testdouble=>create( 'ZIF_ZABAP_TABLE_EDIT_RESTR_SEL' ) ).
    cl_abap_testdouble=>create( 'ZIF_ZABAP_TABLE_EDIT_RESTR_SEL' ).
    cl_abap_testdouble=>configure_call( selection )->ignore_all_parameters( )->and_expect( )->is_never_called( ).
    selection->display( abap_false ).

    config-show_selection_first = abap_false.
    zcl_zabap_table_edit_fact_inj=>inject_restrict_selection( selection ).

    cut = zcl_zabap_table_edit_factory=>get_table_data( config = config grid = grid ).

    cl_abap_testdouble=>verify_expectations( selection ).
  ENDMETHOD.

  METHOD selection_restricted.
    DATA(selection) = CAST zif_zabap_table_edit_restr_sel( cl_abap_testdouble=>create( 'ZIF_ZABAP_TABLE_EDIT_RESTR_SEL' ) ).
    cl_abap_testdouble=>create( 'ZIF_ZABAP_TABLE_EDIT_RESTR_SEL' ).
    cl_abap_testdouble=>configure_call( selection )->ignore_all_parameters( )->returning(
        VALUE rsds_where_tab( ( |key1 <> '{ mocked_db[ 1 ]-key1 }'| ) ) ).
    selection->get_where_cond(  ).

    zcl_zabap_table_edit_fact_inj=>inject_restrict_selection( selection ).
    cut = zcl_zabap_table_edit_factory=>get_table_data( config = config grid = grid ).

    DATA(cut_base) = CAST zcl_zabap_table_edit_tab_data( cut ).
    ASSIGN cut_base->table-initial_data->* TO FIELD-SYMBOL(<table>).

    cl_abap_unit_assert=>assert_equals( exp = VALUE tt_zabap_te_td_test( ( mocked_db[ 2 ] ) ) act = <table> ).
  ENDMETHOD.

  METHOD invalid_data_restrict_sel.
    configure_db_validation( abap_true ).

    DATA(selection) = CAST zif_zabap_table_edit_restr_sel( cl_abap_testdouble=>create( 'ZIF_ZABAP_TABLE_EDIT_RESTR_SEL' ) ).
    cl_abap_testdouble=>create( 'ZIF_ZABAP_TABLE_EDIT_RESTR_SEL' ).
    cl_abap_testdouble=>configure_call( selection )->ignore_all_parameters( )->returning(
        VALUE rsds_frange_t( ( fieldname = 'KEY1' selopt_t = VALUE #( ( sign = 'E' option = 'EQ' low = 'KEY11' ) ) ) ) ).
    selection->get_field_ranges(  ).

    zcl_zabap_table_edit_fact_inj=>inject_restrict_selection( selection ).
    cut = zcl_zabap_table_edit_factory=>get_table_data( config = config grid = grid ).

    DATA(modified) = VALUE tt_zabap_te_td_test( BASE mocked_db ( VALUE #( key1 = 'KEY11' ) ) ).
    DATA(cut_base) = CAST zcl_zabap_table_edit_tab_data( cut ).
    cut_base->table-modified_data_ext = REF #( modified ).
    cut->validate( IMPORTING result = DATA(result) ).

    cl_abap_unit_assert=>assert_equals( exp = zif_zabap_table_edit_data=>c_validation-not_in_selection act = result ).
  ENDMETHOD.

ENDCLASS.
