INTERFACE zif_zabap_table_edit_text_tab PUBLIC.
  TYPES:
    BEGIN OF t_config,
      table_name         TYPE string,
      change_doc_type    TYPE zabap_change_doc_type,
      disable_text_table TYPE abap_bool,
    END OF t_config.

  METHODS:
    append_additional_fields CHANGING additional_fields TYPE cl_abap_structdescr=>component_table,
    update_text_elements CHANGING extended TYPE REF TO data,
    save IMPORTING initial TYPE REF TO data extended TYPE REF TO data,
    lock_table EXPORTING error_message TYPE string RETURNING VALUE(locked) TYPE abap_bool,
    unlock_table.
ENDINTERFACE.
