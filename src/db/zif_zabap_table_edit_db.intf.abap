INTERFACE zif_zabap_table_edit_db PUBLIC.
  METHODS:
    insert_data IMPORTING table TYPE string table_data TYPE table,
    modify_data IMPORTING table TYPE string table_data TYPE table,
    delete_data IMPORTING table TYPE string table_data TYPE table,
    delete_data_where IMPORTING table TYPE string where TYPE string.
ENDINTERFACE.
