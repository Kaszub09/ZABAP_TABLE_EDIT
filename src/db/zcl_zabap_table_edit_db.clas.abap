CLASS zcl_zabap_table_edit_db DEFINITION PUBLIC CREATE PRIVATE GLOBAL FRIENDS zcl_zabap_table_edit_factory.

  PUBLIC SECTION.
    INTERFACES:
      zif_zabap_table_edit_db.

  PROTECTED SECTION.

  PRIVATE SECTION.
ENDCLASS.

CLASS zcl_zabap_table_edit_db IMPLEMENTATION.
  METHOD zif_zabap_table_edit_db~delete_data.
    DELETE (table) FROM TABLE @table_data.
  ENDMETHOD.

  METHOD zif_zabap_table_edit_db~delete_data_where.
    DELETE FROM (table) WHERE (where).
  ENDMETHOD.

  METHOD zif_zabap_table_edit_db~insert_data.
    INSERT (table) FROM TABLE @table_data ACCEPTING DUPLICATE KEYS.
  ENDMETHOD.

  METHOD zif_zabap_table_edit_db~modify_data.
    MODIFY (table) FROM TABLE @table_data.
  ENDMETHOD.
ENDCLASS.
