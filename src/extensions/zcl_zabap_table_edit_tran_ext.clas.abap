"! <p class="shorttext synchronized" lang="en">Allow user to add entries to transport</p>
CLASS zcl_zabap_table_edit_tran_ext DEFINITION PUBLIC FINAL CREATE PUBLIC.

  PUBLIC SECTION.
    INTERFACES:
      zif_zabap_table_edit_commands,
      zif_zabap_table_edit_config,
      zif_zabap_table_edit_data.

  PRIVATE SECTION.
    CONSTANTS:
      BEGIN OF c_commands,
        transport TYPE  syst_ucomm VALUE 'TRANSPORT',
      END OF c_commands.

    TYPES:
        tt_string_table TYPE STANDARD TABLE OF string WITH EMPTY KEY.

    DATA:
      grid              TYPE REF TO cl_gui_alv_grid,
      table_name        TYPE string,
      fields            TYPE REF TO zcl_zabap_table_fields,
      modified_data_ext TYPE REF TO data.
ENDCLASS.

CLASS zcl_zabap_table_edit_tran_ext IMPLEMENTATION.
  METHOD zif_zabap_table_edit_config~grid_setup.
    me->grid = grid->grid.
  ENDMETHOD.

  METHOD zif_zabap_table_edit_data~refresh_grid.
    layout-sel_mode = 'D'.
    me->modified_data_ext = modified_data_ext.
  ENDMETHOD.

  METHOD zif_zabap_table_edit_commands~change_commands.
    APPEND VALUE #( command = c_commands-transport description = VALUE #( icon_id = '@4A@' text = TEXT-f01 icon_text = TEXT-f01 )  ) TO commands.
  ENDMETHOD.

  METHOD zif_zabap_table_edit_commands~before_command.
    IF command <> c_commands-transport.
      RETURN.
    ENDIF.

    grid->get_selected_rows( IMPORTING et_row_no = DATA(rows) ).
    IF lines( rows ) = 0.
      MESSAGE TEXT-001 TYPE 'S' DISPLAY LIKE 'E'.
      RETURN.
    ENDIF.

    "Get keys --------------------
    DATA: keys TYPE tt_string_table.
    FIELD-SYMBOLS: <table> TYPE table.
    ASSIGN modified_data_ext->* TO <table>.

    "Create key struct to cast to cdtabkey - needed to extract just key fields
    fields->get_keys_structure( EXPORTING include_index_field = abap_false IMPORTING struct = DATA(key_struct) ).
    DATA key_line TYPE REF TO data.
    CREATE DATA key_line TYPE HANDLE key_struct.
    FIELD-SYMBOLS <key_line> TYPE any.
    ASSIGN key_line->* TO <key_line>.

    DATA tabkey TYPE string.
    LOOP AT rows REFERENCE INTO data(row).
      <key_line> = CORRESPONDING #( <table>[ row->row_id ] ).
      tabkey = <key_line>.
      APPEND tabkey TO keys.
    ENDLOOP.


    "Transport --------------------
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
      RETURN.
    ENDIF.
    MESSAGE TEXT-002 TYPE 'S'.
  ENDMETHOD.

  METHOD zif_zabap_table_edit_config~change_config.
    table_name = config-table_name.
    fields = NEW #( table_name ).
  ENDMETHOD.

ENDCLASS.
