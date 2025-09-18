"! <p class="shorttext synchronized" lang="en">Allows to set table as editable even if it's standard</p>
CLASS zcl_zabap_table_edit_edit_ext DEFINITION PUBLIC CREATE PUBLIC.

  PUBLIC SECTION.
    INTERFACES:
      zif_zabap_table_edit_config.
ENDCLASS.


CLASS zcl_zabap_table_edit_edit_ext IMPLEMENTATION.
  METHOD zif_zabap_table_edit_config~change_config.
    config-disable_editing = abap_false.
  ENDMETHOD.
ENDCLASS.
