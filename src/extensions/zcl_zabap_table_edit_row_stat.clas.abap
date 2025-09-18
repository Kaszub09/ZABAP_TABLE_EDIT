"! <p class="shorttext synchronized">Allows for easy display of row errors
CLASS zcl_zabap_table_edit_row_stat DEFINITION PUBLIC INHERITING FROM zcl_zabap_table_edit_chain_ext CREATE PUBLIC.

  PUBLIC SECTION.
    METHODS:
      constructor,
      zif_zabap_table_edit_data~additional_fields REDEFINITION,
      zif_zabap_table_edit_data~refresh_grid REDEFINITION,
      zif_zabap_table_edit_config~grid_setup REDEFINITION.

  PROTECTED SECTION.
    TYPES:
      t_exception_col TYPE c LENGTH 1,
      tt_field        TYPE STANDARD TABLE OF fieldname WITH EMPTY KEY.

    CONSTANTS:
      BEGIN OF c_status,
        empty   TYPE t_exception_col VALUE '0',
        error   TYPE t_exception_col VALUE '1',
        warning TYPE t_exception_col VALUE '2',
        success TYPE t_exception_col VALUE '3',
      END OF c_status,
      BEGIN OF c_color,
        BEGIN OF yellow,
          col TYPE  lvc_s_colo-col VALUE 3,
        END OF yellow,
        BEGIN OF blue_key,
          col TYPE  lvc_s_colo-col VALUE 4,
        END OF blue_key,
        BEGIN OF green,
          col TYPE  lvc_s_colo-col VALUE 5,
        END OF green,
        BEGIN OF red,
          col TYPE  lvc_s_colo-col VALUE 6,
        END OF red,
      END OF c_color.

    METHODS:
      clear_all_rows,
      mark_row IMPORTING index  TYPE i
                         status TYPE t_exception_col DEFAULT c_status-empty
                         msg    TYPE string DEFAULT space
                         color  TYPE lvc_t_scol OPTIONAL,
      set_status IMPORTING index TYPE i status TYPE t_exception_col DEFAULT c_status-empty,
      add_to_msg IMPORTING index TYPE i msg TYPE string,
      add_to_color IMPORTING index TYPE i fname TYPE lvc_fname color TYPE lvc_s_colo,
      refresh_grid.

  PRIVATE SECTION.
    CONSTANTS:
      BEGIN OF c_field,
        status TYPE fieldname VALUE 'ZABAP_TABLE_EDIT_EXT_STATUS',
        msg    TYPE fieldname VALUE 'ZABAP_TABLE_EDIT_EXT_MSG',
        color  TYPE fieldname VALUE 'ZABAP_TABLE_EDIT_EXT_COLOR',
      END OF c_field.

    DATA:
      modified_data_ext TYPE REF TO data,
      grid              TYPE REF TO cl_gui_alv_grid.
ENDCLASS.

CLASS zcl_zabap_table_edit_row_stat IMPLEMENTATION.
  METHOD constructor.
    super->constructor( ).
  ENDMETHOD.

  METHOD zif_zabap_table_edit_data~additional_fields.
    APPEND VALUE #( name = c_field-status type = CAST #( cl_abap_typedescr=>describe_by_name( 'CHAR01' ) ) ) TO additional_fields.
    APPEND VALUE #( name = c_field-color type = CAST #( cl_abap_typedescr=>describe_by_name( 'LVC_T_SCOL' ) ) ) TO additional_fields.
    APPEND VALUE #( name = c_field-msg type = CAST #( cl_abap_typedescr=>describe_by_name( 'CHAR128' ) ) ) TO additional_fields.
  ENDMETHOD.

  METHOD zif_zabap_table_edit_data~refresh_grid.
    me->modified_data_ext = modified_data_ext.

    IF in_edit_mode = abap_true.
        DATA(msg_ref) = REF #( field_catalogue[ KEY name fieldname = c_field-msg ] ).
        msg_ref->reptext = TEXT-001.
        msg_ref->scrtext_l = msg_ref->reptext.
        msg_ref->scrtext_m = msg_ref->scrtext_l.
        msg_ref->scrtext_s = msg_ref->scrtext_m.

        layout-ctab_fname = c_field-color.
        layout-excp_fname = c_field-status.
        layout-excp_group = '2'.

        field_catalogue[ KEY name fieldname = c_field-status ]-edit = abap_false.
        field_catalogue[ KEY name fieldname = c_field-msg ]-edit = abap_false.

    ELSE.
      field_catalogue[ KEY name fieldname = c_field-status ]-tech = abap_true.
      field_catalogue[ KEY name fieldname = c_field-msg ]-tech = abap_true.
    ENDIF.
  ENDMETHOD.

  METHOD clear_all_rows.
    FIELD-SYMBOLS: <table> TYPE STANDARD TABLE.

    ASSIGN modified_data_ext->* TO <table>.
    LOOP AT <table> ASSIGNING FIELD-SYMBOL(<row>).
      ASSIGN COMPONENT c_field-status OF STRUCTURE <row> TO FIELD-SYMBOL(<status>).
      ASSIGN COMPONENT c_field-color OF STRUCTURE <row> TO FIELD-SYMBOL(<color>).
      ASSIGN COMPONENT c_field-msg OF STRUCTURE <row> TO FIELD-SYMBOL(<msg>).
      CLEAR: <msg>, <color>, <status>.
    ENDLOOP.
  ENDMETHOD.

  METHOD zif_zabap_table_edit_config~grid_setup.
    me->grid = grid->grid.
  ENDMETHOD.

  METHOD mark_row.
    FIELD-SYMBOLS: <table> TYPE STANDARD TABLE.

    ASSIGN modified_data_ext->* TO <table>.
    ASSIGN <table>[ index ] TO FIELD-SYMBOL(<row>).

    ASSIGN COMPONENT c_field-status OF STRUCTURE <row> TO FIELD-SYMBOL(<status>).
    ASSIGN COMPONENT c_field-color OF STRUCTURE <row> TO FIELD-SYMBOL(<color>).
    ASSIGN COMPONENT c_field-msg OF STRUCTURE <row> TO FIELD-SYMBOL(<msg>).

    <msg> = msg.
    <status> = status.
    <color> = color.
  ENDMETHOD.

  METHOD add_to_color.
    get_row index <row>.
    FIELD-SYMBOLS: <color> TYPE lvc_t_scol.
    ASSIGN COMPONENT c_field-color OF STRUCTURE <row> TO <color>.
    APPEND VALUE #( fname = fname color = color ) TO <color>.
  ENDMETHOD.

  METHOD add_to_msg.
    get_row index <row>.
    ASSIGN COMPONENT c_field-msg OF STRUCTURE <row> TO FIELD-SYMBOL(<msg>).
    <msg> = |{ <msg> } { msg }|.
  ENDMETHOD.

  METHOD set_status.
    get_row index <row>.
    ASSIGN COMPONENT c_field-status OF STRUCTURE <row> TO FIELD-SYMBOL(<status>).
    <status> = status.
  ENDMETHOD.

  METHOD refresh_grid.
    grid->refresh_table_display( ).
  ENDMETHOD.
ENDCLASS.
