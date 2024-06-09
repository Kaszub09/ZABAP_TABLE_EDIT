TYPES:
  tt_zabap_te_test_tt TYPE STANDARD TABLE OF zabap_te_test_tt WITH EMPTY KEY,
  BEGIN OF t_base_extended.
    INCLUDE TYPE zabap_te_test.
  TYPES:
    description1 TYPE zabap_te_test_tt-description1,
    description2 TYPE zabap_te_test_tt-description2,
  END OF t_base_extended,
  tt_base_extended TYPE STANDARD TABLE OF t_base_extended WITH EMPTY KEY.

CONSTANTS:
  c_field      TYPE fieldname  VALUE 'FIELD',
  c_table      TYPE string VALUE 'ZABAP_TE_TEST',
  c_text_table TYPE string VALUE 'ZABAP_TE_TEST_TT'.
"=================================================================
CLASS lcl_test_data DEFINITION CREATE PUBLIC FOR TESTING.
  PUBLIC SECTION.
    INTERFACES:
      zif_zabap_table_edit_db.

    METHODS:
      get_data EXPORTING initial  TYPE REF TO data extended  TYPE REF TO data.

    DATA:
      initial_tab  TYPE tt_base_extended,
      extended_tab TYPE tt_base_extended.
ENDCLASS.

CLASS lcl_test_data IMPLEMENTATION.
  METHOD get_data.
    initial_tab = VALUE tt_base_extended( mandt = sy-mandt
        ( field_name = 'DELETED' ) ( field_name = 'MODIFIED_1' description2 = 'DES2' ) ( field_name = 'MODIFIED_2' ) ).
    extended_tab = VALUE tt_base_extended( mandt = sy-mandt
        ( field_name = 'INSERTED' description1 = 'INS_DES1' ) ( field_name = 'MODIFIED_1' ) ( field_name = 'MODIFIED_2' description2 = 'DES2' ) ).

    initial = REF #( initial_tab ).
    extended = REF #( extended_tab ).
  ENDMETHOD.

  METHOD zif_zabap_table_edit_db~delete_data.
    cl_abap_unit_assert=>fail( |Deletion should be via where clause| ).
  ENDMETHOD.

  METHOD zif_zabap_table_edit_db~delete_data_where.
    cl_abap_unit_assert=>assert_equals( exp = c_text_table act = table ).
    cl_abap_unit_assert=>assert_equals(
        exp = |ZABAP_TE_TEST_TT~TABLE_NAME_TT = @row-TABLE_NAME_TT AND ZABAP_TE_TEST_TT~FIELD_NAME_TT = @row-FIELD_NAME_TT|
        act = where ).
  ENDMETHOD.

  METHOD zif_zabap_table_edit_db~insert_data.
    cl_abap_unit_assert=>assert_equals( exp = c_text_table act = table ).
  ENDMETHOD.

  METHOD zif_zabap_table_edit_db~modify_data.
    cl_abap_unit_assert=>assert_equals( exp = c_text_table act = table ).
    IF lines( table_data ) = 1.
      DATA(expected) = VALUE tt_zabap_te_test_tt( mandt = sy-mandt spras = sy-langu field_name_tt = 'INSERTED' ( description1 = 'INS_DES1' )  ).
      cl_abap_unit_assert=>assert_equals( exp = expected act = table_data ).
    ELSEIF lines( table_data ) = 2.
      DATA(expected2) = VALUE tt_zabap_te_test_tt( mandt = sy-mandt spras = sy-langu
        ( field_name_tt = 'MODIFIED_1'  ) ( field_name_tt = 'MODIFIED_2' description2 = 'DES2' )   ).
      cl_abap_unit_assert=>assert_equals( exp = expected2 act = table_data ).
    ELSE.
      cl_abap_unit_assert=>fail( |uinexpected number of rows| ).
    ENDIF.
  ENDMETHOD.
ENDCLASS.
"=================================================================
CLASS tcl_zabap_table_edit_text_tab DEFINITION DEFERRED.
CLASS zcl_zabap_table_edit_text_tab DEFINITION LOCAL FRIENDS tcl_zabap_table_edit_text_tab.
CLASS tcl_zabap_table_edit_text_tab DEFINITION FINAL FOR TESTING RISK LEVEL HARMLESS DURATION SHORT.

  PRIVATE SECTION.
    CLASS-METHODS:
      class_setup,
      class_teardown.

    METHODS:
      setup,
      append_fields        FOR TESTING,
      update_text_elements FOR TESTING,
      correct_saved        FOR TESTING,
      cd_called_correctly FOR TESTING,
      can_lock FOR TESTING.

    DATA:
        cut TYPE REF TO zif_zabap_table_edit_text_tab.
ENDCLASS.

CLASS tcl_zabap_table_edit_text_tab IMPLEMENTATION.
  METHOD class_setup.
    DATA(zabap_te_test_tt_data) = VALUE tt_zabap_te_test_tt( mandt = sy-mandt field_name_tt = c_field
        ( spras = sy-langu description1 = 'DES1' description2 = 'DES2' ) ( spras = 'X' description1 = 'DES1_X' description2 = 'DES2_X' ) ).

    MODIFY zabap_te_test_tt FROM TABLE @zabap_te_test_tt_data.
  ENDMETHOD.

  METHOD class_teardown.
    ROLLBACK WORK.
  ENDMETHOD.

  METHOD setup.
    zcl_zabap_table_edit_fact_inj=>clear_injected_cd( ).
    zcl_zabap_table_edit_fact_inj=>clear_injected_db( ).
    cut = zcl_zabap_table_edit_factory=>get_text_table( config = VALUE #( table_name = c_table ) ).
  ENDMETHOD.

  METHOD append_fields.
    DATA zabap_te_test_tt TYPE zabap_te_test_tt.

    DATA(initial) = VALUE cl_abap_structdescr=>component_table( ).
    DATA(expected) = VALUE cl_abap_structdescr=>component_table(
      ( name = 'DESCRIPTION1' type = CAST #( cl_abap_elemdescr=>describe_by_data( zabap_te_test_tt-description1 ) ) )
      ( name = 'DESCRIPTION2' type = CAST #( cl_abap_elemdescr=>describe_by_data( zabap_te_test_tt-description2 ) ) ) ).

    cut->append_additional_fields( CHANGING additional_fields = initial ).

    cl_abap_unit_assert=>assert_equals( exp = expected act = initial ).
  ENDMETHOD.

  METHOD update_text_elements.
    DATA(extended) = VALUE tt_base_extended( mandt = sy-mandt field_name = c_field ( ) ( table_name = 'DUMMY' ) ).
    DATA(extended_ref) = REF data( extended ).
    cut->update_text_elements( CHANGING extended = extended_ref ).

    cl_abap_unit_assert=>assert_equals( exp = 'DES1' act = extended[ 1 ]-description1 ).
    cl_abap_unit_assert=>assert_equals( exp = 'DES2' act = extended[ 1 ]-description2 ).
    cl_abap_unit_assert=>assert_equals( exp = space act = extended[ 2 ]-description1 ).
  ENDMETHOD.

  METHOD correct_saved.
    DATA(test_data) = NEW lcl_test_data( ).
    zcl_zabap_table_edit_fact_inj=>inject_db( test_data ).
    cut = zcl_zabap_table_edit_factory=>get_text_table( config = VALUE #( table_name = c_table ) ).

    test_data->get_data( IMPORTING initial = DATA(initial) extended = DATA(extended) ).
    cut->save( initial = initial extended = extended ).
  ENDMETHOD.

  METHOD cd_called_correctly.
    DATA cd_states TYPE TABLE OF c.
    cd_states = VALUE #( ( space ) ( 'F' ) ( 'X' ) ).
    LOOP AT cd_states REFERENCE INTO DATA(cd_state).
      zcl_zabap_table_edit_fact_inj=>inject_cd( NEW zcl_zabap_table_edit_cd_test( cd_state->* ) ).
      DATA(test_data) = NEW lcl_test_data( ).
      zcl_zabap_table_edit_fact_inj=>inject_db( test_data ).

      cut = zcl_zabap_table_edit_factory=>get_text_table( config = VALUE #( table_name = c_table change_doc_type = cd_state->* ) ).

      test_data->get_data( IMPORTING initial = DATA(initial) extended = DATA(extended) ).
      cut->save( initial = initial extended = extended ).
    ENDLOOP.

  ENDMETHOD.

  METHOD can_lock.
    cl_abap_unit_assert=>assert_true( cut->lock_table( ) ).
    cut->unlock_table( ).
  ENDMETHOD.

ENDCLASS.
