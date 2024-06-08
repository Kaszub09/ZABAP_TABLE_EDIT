CLASS zcl_zabap_table_edit_fact_inj DEFINITION PUBLIC FINAL CREATE PUBLIC FOR TESTING.

  PUBLIC SECTION.
    CLASS-METHODS:
        inject_db IMPORTING db_interface TYPE REF TO zif_zabap_table_edit_db.
ENDCLASS.

CLASS zcl_zabap_table_edit_fact_inj IMPLEMENTATION.
  METHOD inject_db.
    zcl_zabap_table_edit_factory=>db = db_interface.
  ENDMETHOD.
ENDCLASS.
