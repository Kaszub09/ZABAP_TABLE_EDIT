CLASS tcl_zabap_table_comparator DEFINITION FINAL FOR TESTING RISK LEVEL HARMLESS DURATION SHORT.
  PRIVATE SECTION.
    TYPES:
      tt_zabap_te_cp_test TYPE STANDARD TABLE OF zabap_te_cp_test WITH EMPTY KEY.

    METHODS:
      setup,
      update_mandant FOR TESTING,
      detect_duplicates FOR TESTING,
      detect_inserted FOR TESTING,
      detect_deleted FOR TESTING,
      detect_modified FOR TESTING,
      detect_changes_to_empty FOR TESTING,
      detect_all_deleted FOR TESTING,
      detect_records_swapped FOR TESTING,
      detect_records_swapped_inv FOR TESTING,
      assert_table_is_equal IMPORTING expected TYPE tt_zabap_te_cp_test act TYPE REF TO data,
      assert_table_is_empty IMPORTING table_data TYPE REF TO data,
      when_tables_are_compared.

    CONSTANTS:
      c_table_name  TYPE string VALUE 'ZABAP_TE_CP_TEST'.

    DATA:
      cut             TYPE REF TO zcl_zabap_table_comparator,
      initial_data    TYPE tt_zabap_te_cp_test,
      modified_data   TYPE tt_zabap_te_cp_test,
      duplicates      TYPE REF TO data,
      inserted        TYPE REF TO data,
      deleted         TYPE REF TO data,
      before_modified TYPE REF TO data,
      modified        TYPE REF TO data.
ENDCLASS.

CLASS tcl_zabap_table_comparator IMPLEMENTATION.
  METHOD setup.
    cut = NEW #( c_table_name ).
  ENDMETHOD.

  METHOD update_mandant.
    DATA(initial) = VALUE tt_zabap_te_cp_test( ( key1 = 'K1' ) ( mandt = sy-mandt key1 = 'K2' ) ( key1 = 'K3' ) ).
    DATA(expected) = VALUE tt_zabap_te_cp_test( ( mandt = sy-mandt key1 = 'K1' ) ( mandt = sy-mandt key1 = 'K2' ) ( mandt = sy-mandt key1 = 'K3' ) ).

    cut->update_mandant( REF #( initial ) ).

    cl_abap_unit_assert=>assert_equals( act = initial exp = expected ).
  ENDMETHOD.

  METHOD when_tables_are_compared.
    cut->compare_tables( EXPORTING initial_data = REF #( initial_data ) modified_data = REF #( modified_data )
            IMPORTING duplicates = duplicates inserted = inserted deleted = deleted
            before_modified = before_modified modified = modified ).
  ENDMETHOD.


  METHOD assert_table_is_empty.
    assign_to_table_fs table_data->* <table_data>.
    cl_abap_unit_assert=>assert_equals( exp = 0 act = lines( <table_data> ) ).
  ENDMETHOD.

  METHOD assert_table_is_equal.
    assign_to_table_fs act->* <act>.
    cl_abap_unit_assert=>assert_equals( exp = expected act = <act> ).
  ENDMETHOD.


  METHOD detect_deleted.
    "Given entries were deleted
    initial_data = VALUE #( ( key1 = 'K1' ) ( key1 = 'K2' ) ( key1 = 'K3' ) ).
    modified_data = VALUE #( ( initial_data[ 2 ] ) ).

    when_tables_are_compared( ).

    "Expect
    assert_table_is_equal( expected = VALUE #( ( initial_data[ 1 ] ) ( initial_data[ 3 ] ) ) act = deleted ).
    assert_table_is_empty( duplicates ).
    assert_table_is_empty( inserted ).
    assert_table_is_empty( before_modified ).
    assert_table_is_empty( modified ).
  ENDMETHOD.

  METHOD detect_duplicates.
    "Given duplicate entries were inserted
    initial_data = VALUE #( ( key1 = 'K2' ) ( key1 = 'K3' ) ).
    modified_data = VALUE #( ( key1 = 'K3' ) ( key1 = 'K2' ) ( key1 = 'K2' val1 = 'VAL1' ) ( key1 = 'K2' ) ).

    when_tables_are_compared( ).

    "Expect
    assert_table_is_equal( expected = VALUE #( ( modified_data[ 3 ] ) ( modified_data[ 4 ] ) ) act = duplicates ).
    assert_table_is_equal( expected = VALUE #( ( modified_data[ 3 ] ) ( modified_data[ 4 ] ) ) act = inserted ).
    assert_table_is_empty( deleted ).
    assert_table_is_empty( before_modified ).
    assert_table_is_empty( modified ).
  ENDMETHOD.

  METHOD detect_inserted.
    "Given entries were inserted
    initial_data = VALUE #( ( key1 = 'K2' ) ).
    modified_data = VALUE #( ( key1 = 'K1' ) ( key1 = 'K2' ) ( key1 = 'K3' ) ).

    when_tables_are_compared( ).

    "Expect
    assert_table_is_equal( expected = VALUE #( ( modified_data[ 1 ] ) ( modified_data[ 3 ] ) ) act = inserted ).
    assert_table_is_empty( duplicates ).
    assert_table_is_empty( deleted ).
    assert_table_is_empty( before_modified ).
    assert_table_is_empty( modified ).
  ENDMETHOD.

  METHOD detect_modified.
    "Given entries were modified
    initial_data = VALUE #( ( key1 = 'K1' ) ( key1 = 'K2' val1 = 'VAL2' ) ( key1 = 'K3' val1 = 'VAL3' ) ).
    modified_data = VALUE #( ( key1 = 'K2' ) ( key1 = 'K1' val1 = 'VAL1' ) ( key1 = 'K3' val1 = 'VAL' ) ).

    when_tables_are_compared( ).

    "Expect
    assert_table_is_equal( expected = VALUE #( ( initial_data[ 1 ] ) ( initial_data[ 2 ] ) ( initial_data[ 3 ] ) ) act = before_modified ).
    assert_table_is_equal( expected = VALUE #( ( modified_data[ 2 ] ) ( modified_data[ 1 ] ) ( modified_data[ 3 ] ) ) act = modified ).
    assert_table_is_empty( duplicates ).
    assert_table_is_empty( deleted ).
    assert_table_is_empty( inserted ).
  ENDMETHOD.

  METHOD detect_changes_to_empty.
    "Given table was initially empty
    initial_data = VALUE #( ).
    modified_data = VALUE #( ( key1 = 'K2' ) ( key1 = 'K2' val1 = 'VAL1' ) ( key1 = 'K3' val1 = 'VAL' ) ).

    when_tables_are_compared( ).

    "Expect
    assert_table_is_equal( expected = VALUE #( ( modified_data[ 2 ] ) ) act = duplicates ).
    assert_table_is_equal( expected = VALUE #( ( modified_data[ 1 ] ) ( modified_data[ 2 ] ) ( modified_data[ 3 ] ) ) act = inserted ).
    assert_table_is_empty( deleted ).
    assert_table_is_empty( modified ).
    assert_table_is_empty( before_modified ).
  ENDMETHOD.

  METHOD detect_all_deleted.
    "Given everything was deleted
    initial_data = VALUE #( ( key1 = 'K1' ) ( key1 = 'K2' val1 = 'VAL1' ) ( key1 = 'K3' val1 = 'VAL' ) ).
    modified_data = VALUE #( ).

    when_tables_are_compared( ).

    "Expect
    assert_table_is_equal( expected = initial_data act = deleted ).
    assert_table_is_empty( duplicates ).
    assert_table_is_empty( inserted ).
    assert_table_is_empty( modified ).
    assert_table_is_empty( before_modified ).
  ENDMETHOD.

  METHOD detect_records_swapped.
    "Given records swapped - needed to cover some specific execution path with 'THEN initial_data_index'
    initial_data = VALUE #( ( key1 = 'K1' ) ).
    modified_data = VALUE #( ( key1 = 'K2' ) ).

    when_tables_are_compared( ).

    "Expect
    assert_table_is_equal( expected = initial_data act = deleted ).
    assert_table_is_equal( expected = modified_data act = inserted ).
    assert_table_is_empty( duplicates ).
    assert_table_is_empty( modified ).
    assert_table_is_empty( before_modified ).
  ENDMETHOD.

  METHOD detect_records_swapped_inv.
    "Given records swapped - needed to cover some specific execution path - 'THEN modified_data_index'
    initial_data = VALUE #( ( key1 = 'K2' ) ).
    modified_data = VALUE #( ( key1 = 'K1' ) ).

    when_tables_are_compared( ).

    "Expect
    assert_table_is_equal( expected = initial_data act = deleted ).
    assert_table_is_equal( expected = modified_data act = inserted ).
    assert_table_is_empty( duplicates ).
    assert_table_is_empty( modified ).
    assert_table_is_empty( before_modified ).
  ENDMETHOD.

ENDCLASS.
