CLASS zcl_zabap_table_edit_factory DEFINITION PUBLIC CREATE PUBLIC.

  PUBLIC SECTION.
    CLASS-METHODS:
      get_text_table IMPORTING config            TYPE zif_zabap_table_edit_text_tab=>t_config
                     RETURNING VALUE(text_table) TYPE REF TO zif_zabap_table_edit_text_tab,
      get_maintanance_view IMPORTING view TYPE string  RETURNING VALUE(maintanance_view) TYPE REF TO zif_zabap_table_edit_mview.

  PRIVATE SECTION.
    CLASS-DATA:
        mock_text_table TYPE REF TO zif_zabap_table_edit_text_tab.
ENDCLASS.


CLASS zcl_zabap_table_edit_factory IMPLEMENTATION.
  METHOD get_text_table.
    "To avoid doing too many levels of abstraction for the sole purpose of testing
    IF mock_text_table IS BOUND.
      text_table = mock_text_table.
      RETURN.
    ENDIF.
    "--------------------------------------------------
    SELECT SINGLE @abap_true AS exists FROM dd08l
      WHERE checktable = @config-table_name AND frkart = 'TEXT'
      INTO @DATA(exists).

    IF exists = abap_true AND config-disable_text_table = abap_false.
      text_table = NEW zcl_zabap_table_edit_text_tab( config ).
    ELSE.
      text_table = NEW zcl_zabap_table_edit_txt_tab_e( ).
    ENDIF.
  ENDMETHOD.

  METHOD get_maintanance_view.
    " TODO: parameter VIEW is never used (ABAP cleaner)

    maintanance_view = NEW zcl_zabap_table_edit_mview_e( ).
  ENDMETHOD.
ENDCLASS.
