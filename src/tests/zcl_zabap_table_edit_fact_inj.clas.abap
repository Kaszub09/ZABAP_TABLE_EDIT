CLASS zcl_zabap_table_edit_fact_inj DEFINITION PUBLIC CREATE PRIVATE FOR TESTING.

  PUBLIC SECTION.
    CLASS-METHODS:
      inject_db IMPORTING db_interface TYPE REF TO zif_zabap_table_edit_db,
      clear_injected_db,
      inject_cd IMPORTING cd TYPE REF TO zif_zabap_change_document,
      clear_injected_cd,
      inject_table_data IMPORTING table_data TYPE REF TO zif_zabap_table_edit_tab_data,
      clear_injected_table_data,
      inject_grid IMPORTING grid TYPE REF TO zif_zabap_table_edit_grid_if,
      clear_injected_grid.
ENDCLASS.

CLASS zcl_zabap_table_edit_fact_inj IMPLEMENTATION.
  METHOD inject_db.
    zcl_zabap_table_edit_factory=>db = db_interface.
  ENDMETHOD.

  METHOD clear_injected_db.
    CLEAR zcl_zabap_table_edit_factory=>db.
  ENDMETHOD.

  METHOD clear_injected_cd.
    CLEAR zcl_zabap_table_edit_factory=>mock_change_doc.
  ENDMETHOD.

  METHOD inject_cd.
    zcl_zabap_table_edit_factory=>mock_change_doc = cd.
  ENDMETHOD.
  METHOD clear_injected_table_data.
    CLEAR zcl_zabap_table_edit_factory=>mock_table_data.
  ENDMETHOD.

  METHOD inject_table_data.
    zcl_zabap_table_edit_factory=>mock_table_data = table_data.
  ENDMETHOD.

  METHOD clear_injected_grid.
    CLEAR zcl_zabap_table_edit_factory=>mock_grid.
  ENDMETHOD.

  METHOD inject_grid.
    zcl_zabap_table_edit_factory=>mock_grid = grid.
  ENDMETHOD.

ENDCLASS.
