CLASS zcl_zabap_table_edit_factory DEFINITION PUBLIC CREATE PRIVATE GLOBAL FRIENDS zcl_zabap_table_edit_fact_inj.

  PUBLIC SECTION.
    CLASS-METHODS:
      get_text_table IMPORTING config            TYPE zif_zabap_table_edit_text_tab=>t_config
                     RETURNING VALUE(text_table) TYPE REF TO zif_zabap_table_edit_text_tab,
      get_maintanance_view IMPORTING view TYPE string RETURNING VALUE(maintanance_view) TYPE REF TO zif_zabap_table_edit_mview,
      get_db RETURNING VALUE(db_interface) TYPE REF TO zif_zabap_table_edit_db,
      "! @parameter objectclass | <p class="shorttext synchronized">Name of CD object (e.g. from SCDO)</p>
      "! @parameter objectid |  <p class="shorttext synchronized">Object ID inside CD object, e.g. matnr for MATERIAL class...</p>
      "! Something that ties records from all tables in SCDO, like common foreign key, e.g. vbeln for ekko/ekpo
      get_change_doc IMPORTING objectclass TYPE cdobjectcl objectid TYPE cdobjectv
                     RETURNING VALUE(change_doc) TYPE REF TO zif_zabap_change_document,
      get_table_data IMPORTING config TYPE zif_zabap_table_edit_tab_data=>t_config grid TYPE REF TO zif_zabap_table_edit_grid_if
                     RETURNING VALUE(table_data) TYPE REF TO zif_zabap_table_edit_tab_data,
      get_grid IMPORTING container TYPE REF TO cl_gui_container RETURNING VALUE(grid) TYPE REF TO zif_zabap_table_edit_grid_if,
      get_restrict_selection IMPORTING config TYPE zcl_zabap_table_edit_restr_sel=>t_config
                             RETURNING VALUE(selection) TYPE REF TO zif_zabap_table_edit_restr_sel.

  PRIVATE SECTION.
    CLASS-DATA:
      mock_grid       TYPE REF TO zif_zabap_table_edit_grid_if,
      mock_table_data TYPE REF TO zif_zabap_table_edit_tab_data,
      mock_text_table TYPE REF TO zif_zabap_table_edit_text_tab,
      mock_change_doc TYPE REF TO zif_zabap_change_document,
      mock_selection  TYPE REF TO zif_zabap_table_edit_restr_sel,
      db              TYPE REF TO zif_zabap_table_edit_db.
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
    maintanance_view = NEW zcl_zabap_table_edit_mview_e( ).
  ENDMETHOD.

  METHOD get_db.
    IF NOT db IS BOUND.
      db = NEW zcl_zabap_table_edit_db( ).
    ENDIF.
    db_interface = db.
  ENDMETHOD.

  METHOD get_change_doc.
    "To avoid doing too many levels of abstraction for the sole purpose of testing
    IF mock_change_doc IS BOUND.
      change_doc = mock_change_doc.
      RETURN.
    ENDIF.
    "--------------------------------------------------
    change_doc = NEW zcl_zabap_change_document( objectclass = objectclass objectid = objectid ).
  ENDMETHOD.

  METHOD get_table_data.
    "To avoid doing too many levels of abstraction for the sole purpose of testing
    IF mock_table_data IS BOUND.
      table_data = mock_table_data.
      RETURN.
    ENDIF.
    "--------------------------------------------------
    table_data = NEW zcl_zabap_table_edit_tab_data( configuration = config grid = grid ).
  ENDMETHOD.
  METHOD get_grid.
    "To avoid doing too many levels of abstraction for the sole purpose of testing
    IF mock_grid IS BOUND.
      grid = mock_grid.
      RETURN.
    ENDIF.
    "--------------------------------------------------
    grid = NEW zcl_zabap_table_edit_grid( container ).
  ENDMETHOD.

  METHOD get_restrict_selection.
    "To avoid doing too many levels of abstraction for the sole purpose of testing
    IF mock_selection IS BOUND.
      selection = mock_selection.
      RETURN.
    ENDIF.
    "--------------------------------------------------
    selection = NEW zcl_zabap_table_edit_restr_sel( config ).
  ENDMETHOD.

ENDCLASS.
