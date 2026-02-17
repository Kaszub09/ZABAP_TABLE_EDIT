CLASS zcl_zabap_table_edit_tr_ext DEFINITION PUBLIC FINAL CREATE PUBLIC.

  PUBLIC SECTION.
    INTERFACES:
      zif_zabap_table_edit_commands,
      zif_zabap_table_edit_config,
      zif_zabap_table_edit_data.

    METHODS:
      constructor IMPORTING block_edit_on_non_dev TYPE abap_bool DEFAULT abap_true.

  PRIVATE SECTION.
    TYPES:
        tt_string_table TYPE STANDARD TABLE OF string WITH EMPTY KEY.

    METHODS:
      get_keys IMPORTING table_ref TYPE REF TO data RETURNING VALUE(keys) TYPE tt_string_table.

    DATA:
      allow_edit TYPE abap_bool,
      table_name TYPE string,
      fields     TYPE REF TO zcl_zabap_table_fields.
ENDCLASS.

CLASS zcl_zabap_table_edit_tr_ext IMPLEMENTATION.
  METHOD constructor.
    SELECT SINGLE CASE WHEN cccategory = 'C' THEN @abap_true ELSE @abap_false END AS is_dev FROM t000 INTO @allow_edit.
    allow_edit = xsdbool( allow_edit = abap_true OR block_edit_on_non_dev = abap_false ).
  ENDMETHOD.

  METHOD zif_zabap_table_edit_data~before_save.
    DATA keys TYPE tt_string_table.

    APPEND LINES OF get_keys( compared-inserted ) TO keys.
    APPEND LINES OF get_keys( compared-deleted ) TO keys.
    APPEND LINES OF get_keys( compared-modified ) TO keys.

    DATA:
      wt_ko200 TYPE STANDARD TABLE OF ko200 WITH DEFAULT KEY,
      wt_e071k TYPE STANDARD TABLE OF e071k WITH DEFAULT KEY.

    wt_ko200 = VALUE #( ( pgmid = 'R3TR' object = 'TABU' obj_name = table_name objfunc = 'K' ) ).
    wt_e071k = VALUE #( FOR <key> IN keys ( pgmid = 'R3TR' object = 'TABU' objname = table_name
        mastertype = 'TABU' mastername = table_name tabkey = <key> ) ).

    CALL FUNCTION 'TR_OBJECTS_CHECK'
      TABLES
        wt_ko200                = wt_ko200
        wt_e071k                = wt_e071k
      EXCEPTIONS
        cancel_edit_other_error = 1                " Cancel
        show_only_other_error   = 2                " Cancel, user wants to go to display mode
        OTHERS                  = 3.
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno  WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
      cancel = abap_true.
      RETURN.
    ENDIF.

    CALL FUNCTION 'TR_OBJECTS_INSERT'
      TABLES
        wt_ko200                = wt_ko200
        wt_e071k                = wt_e071k
      EXCEPTIONS
        cancel_edit_other_error = 1
        show_only_other_error   = 2
        OTHERS                  = 3.
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
      cancel = abap_true.
    ENDIF.
  ENDMETHOD.

  METHOD zif_zabap_table_edit_config~change_config.
    table_name = config-table_name.
    fields = NEW #( table_name ).
    IF allow_edit = abap_false.
      config-disable_editing = abap_false.
      MESSAGE TEXT-001 TYPE 'S'.
    ENDIF.
  ENDMETHOD.

  METHOD get_keys.
    FIELD-SYMBOLS <table> TYPE table.

    ASSIGN table_ref->* TO <table>.

    "Create key struct to cast to cdtabkey - needed to extract just key fields
    fields->get_keys_structure( EXPORTING include_index_field = abap_false IMPORTING struct = DATA(key_struct) ).
    DATA key_line TYPE REF TO data.
    CREATE DATA key_line TYPE HANDLE key_struct.
    FIELD-SYMBOLS <key_line> TYPE any.
    ASSIGN key_line->* TO <key_line>.

    DATA tabkey TYPE string.
    LOOP AT <table> ASSIGNING FIELD-SYMBOL(<row>).
      <key_line> = CORRESPONDING #( <row> ).
      tabkey = <key_line>.
      APPEND tabkey TO keys.
    ENDLOOP.
  ENDMETHOD.
ENDCLASS.
