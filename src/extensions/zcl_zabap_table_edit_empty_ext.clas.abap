CLASS zcl_zabap_table_edit_empty_ext DEFINITION PUBLIC FINAL CREATE PUBLIC.

  PUBLIC SECTION.
    INTERFACES:
      zif_zabap_table_edit_commands,
      zif_zabap_table_edit_config,
      zif_zabap_table_edit_data.
ENDCLASS.


CLASS zcl_zabap_table_edit_empty_ext IMPLEMENTATION.
  METHOD zif_zabap_table_edit_data~additional_fields.

  ENDMETHOD.

  METHOD zif_zabap_table_edit_data~additional_validation.

  ENDMETHOD.

  METHOD zif_zabap_table_edit_commands~after_command.

  ENDMETHOD.

  METHOD zif_zabap_table_edit_data~after_save.

  ENDMETHOD.

  METHOD zif_zabap_table_edit_commands~before_command.

  ENDMETHOD.

  METHOD zif_zabap_table_edit_data~before_save.

  ENDMETHOD.

  METHOD zif_zabap_table_edit_commands~change_commands.

  ENDMETHOD.

  METHOD zif_zabap_table_edit_config~change_config.

  ENDMETHOD.

  METHOD zif_zabap_table_edit_data~default_select.

  ENDMETHOD.

  METHOD zif_zabap_table_edit_config~grid_setup.

  ENDMETHOD.

  METHOD zif_zabap_table_edit_data~initial_data.

  ENDMETHOD.

  METHOD zif_zabap_table_edit_data~on_data_changed.

  ENDMETHOD.

  METHOD zif_zabap_table_edit_data~on_data_changed_finished.

  ENDMETHOD.

  METHOD zif_zabap_table_edit_data~refresh_grid.

  ENDMETHOD.

  METHOD zif_zabap_table_edit_commands~set_edit_mode.

  ENDMETHOD.

  METHOD zif_zabap_table_edit_data~change_display_text.

  ENDMETHOD.

ENDCLASS.
