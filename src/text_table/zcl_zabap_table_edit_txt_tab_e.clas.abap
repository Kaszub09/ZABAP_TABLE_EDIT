CLASS zcl_zabap_table_edit_txt_tab_e DEFINITION PUBLIC CREATE PRIVATE
  GLOBAL FRIENDS zcl_zabap_table_edit_factory.

  PUBLIC SECTION.
    INTERFACES:
      zif_zabap_table_edit_text_tab.
ENDCLASS.


CLASS zcl_zabap_table_edit_txt_tab_e IMPLEMENTATION.
  METHOD zif_zabap_table_edit_text_tab~append_additional_fields.
  ENDMETHOD.

  METHOD zif_zabap_table_edit_text_tab~lock_table.
    locked = abap_true.
  ENDMETHOD.

  METHOD zif_zabap_table_edit_text_tab~save.
  ENDMETHOD.

  METHOD zif_zabap_table_edit_text_tab~unlock_table.
  ENDMETHOD.

  METHOD zif_zabap_table_edit_text_tab~update_text_elements.
  ENDMETHOD.
ENDCLASS.
