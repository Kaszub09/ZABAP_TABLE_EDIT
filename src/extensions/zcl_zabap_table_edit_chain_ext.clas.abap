"! <p class="shorttext synchronized" lang="en">Allows for easy chaining of multiple interfaces</p>
"! You can inherit from it and add additional interfaces in constructor, as well as redefine some methods
"! - but remember to call super (base class implementation) inside them
CLASS zcl_zabap_table_edit_chain_ext DEFINITION PUBLIC CREATE PUBLIC.

  PUBLIC SECTION.
    INTERFACES:
      zif_zabap_table_edit_commands,
      zif_zabap_table_edit_config,
      zif_zabap_table_edit_data.
  PROTECTED SECTION.
    METHODS:
      add_all_valid_interfaces IMPORTING interface TYPE REF TO object,
      add_command_interface IMPORTING interface TYPE REF TO zif_zabap_table_edit_commands,
      add_config_interface IMPORTING interface TYPE REF TO zif_zabap_table_edit_config,
      add_data_interface IMPORTING interface TYPE REF TO zif_zabap_table_edit_data.
  PRIVATE SECTION.
    DATA:
      command_interfaces TYPE STANDARD TABLE OF REF TO zif_zabap_table_edit_commands WITH EMPTY KEY,
      config_interfaces  TYPE STANDARD TABLE OF REF TO zif_zabap_table_edit_config WITH EMPTY KEY,
      data_interfaces    TYPE STANDARD TABLE OF REF TO zif_zabap_table_edit_data WITH EMPTY KEY.
ENDCLASS.


CLASS zcl_zabap_table_edit_chain_ext IMPLEMENTATION.
  METHOD zif_zabap_table_edit_data~additional_fields.
    LOOP AT data_interfaces INTO DATA(data_interface).
      data_interface->additional_fields( CHANGING additional_fields = additional_fields ).
    ENDLOOP.
  ENDMETHOD.

  METHOD zif_zabap_table_edit_data~additional_validation.
    LOOP AT data_interfaces INTO DATA(data_interface).
      data_interface->additional_validation( CHANGING result = result all_modified_data = all_modified_data compared = compared ).
    ENDLOOP.
  ENDMETHOD.

  METHOD zif_zabap_table_edit_commands~after_command.
    LOOP AT command_interfaces INTO DATA(command_interface).
      command_interface->after_command( CHANGING command = command refresh_grid = refresh_grid ).
    ENDLOOP.
  ENDMETHOD.

  METHOD zif_zabap_table_edit_data~after_save.
    LOOP AT data_interfaces INTO DATA(data_interface).
      data_interface->after_save( CHANGING compared = compared ).
    ENDLOOP.
  ENDMETHOD.

  METHOD zif_zabap_table_edit_commands~before_command.
    LOOP AT command_interfaces INTO DATA(command_interface).
      command_interface->before_command( CHANGING command = command cancel_command = cancel_command ).
    ENDLOOP.
  ENDMETHOD.

  METHOD zif_zabap_table_edit_data~before_save.
    LOOP AT data_interfaces INTO DATA(data_interface).
      data_interface->before_save( CHANGING compared = compared ).
    ENDLOOP.
  ENDMETHOD.

  METHOD zif_zabap_table_edit_commands~change_commands.
    LOOP AT command_interfaces INTO DATA(command_interface).
      command_interface->change_commands( EXPORTING in_edit_mode = in_edit_mode
                                          CHANGING top_commands = top_commands commands = commands ).
    ENDLOOP.
  ENDMETHOD.



  METHOD zif_zabap_table_edit_config~change_config.
    LOOP AT config_interfaces INTO DATA(config_interface).
      config_interface->change_config( CHANGING config = config ).
    ENDLOOP.
  ENDMETHOD.

  METHOD zif_zabap_table_edit_data~default_select.
    LOOP AT data_interfaces INTO DATA(data_interface).
      data_interface->default_select( CHANGING execute = execute ).
    ENDLOOP.
  ENDMETHOD.

  METHOD zif_zabap_table_edit_config~grid_setup.
    LOOP AT config_interfaces INTO DATA(config_interface).
      config_interface->grid_setup( CHANGING grid = grid ).
    ENDLOOP.
  ENDMETHOD.

  METHOD zif_zabap_table_edit_data~initial_data.
    LOOP AT data_interfaces INTO DATA(data_interface).
      data_interface->initial_data( CHANGING initial_data = initial_data ).
    ENDLOOP.
  ENDMETHOD.

  METHOD zif_zabap_table_edit_data~on_data_changed.
    LOOP AT data_interfaces INTO DATA(data_interface).
      data_interface->on_data_changed( EXPORTING er_data_changed = er_data_changed e_onf4 = e_onf4 e_onf4_before = e_onf4_before
                                                 e_onf4_after = e_onf4_after e_ucomm = e_ucomm sender = sender ).
    ENDLOOP.
  ENDMETHOD.

  METHOD zif_zabap_table_edit_data~on_data_changed_finished.
    LOOP AT data_interfaces INTO DATA(data_interface).
      data_interface->on_data_changed_finished( EXPORTING e_modified = e_modified et_good_cells = et_good_cells sender = sender ).
    ENDLOOP.
  ENDMETHOD.

  METHOD zif_zabap_table_edit_data~refresh_grid.
    LOOP AT data_interfaces INTO DATA(data_interface).
      data_interface->refresh_grid( EXPORTING in_edit_mode = in_edit_mode
                                    CHANGING field_catalogue = field_catalogue initial_data = initial_data
                                             modified_data_ext = modified_data_ext layout = layout variant = variant ).
    ENDLOOP.
  ENDMETHOD.

  METHOD zif_zabap_table_edit_commands~set_edit_mode.
    LOOP AT command_interfaces INTO DATA(command_interface).
      command_interface->set_edit_mode( CHANGING editable = editable ).
    ENDLOOP.
  ENDMETHOD.

  METHOD zif_zabap_table_edit_data~change_display_text.
    LOOP AT data_interfaces INTO DATA(data_interface).
      data_interface->change_display_text( CHANGING display_text = display_text ).
    ENDLOOP.
  ENDMETHOD.

  METHOD add_all_valid_interfaces.
    IF interface IS INSTANCE OF zif_zabap_table_edit_commands.
      add_command_interface( CAST #( interface ) ).
    ENDIF.
    IF interface IS INSTANCE OF zif_zabap_table_edit_config.
      add_config_interface( CAST #( interface ) ).
    ENDIF.
    IF interface IS INSTANCE OF zif_zabap_table_edit_data.
      add_data_interface( CAST #( interface ) ).
    ENDIF.
  ENDMETHOD.

  METHOD add_command_interface.
    APPEND interface TO command_interfaces.
  ENDMETHOD.

  METHOD add_config_interface.
    APPEND interface TO config_interfaces.
  ENDMETHOD.

  METHOD add_data_interface.
    APPEND interface TO data_interfaces.
  ENDMETHOD.

  METHOD zif_zabap_table_edit_config~change_init_selection_fields.
    LOOP AT config_interfaces INTO DATA(config_interface).
      config_interface->change_init_selection_fields( CHANGING fields_tab = fields_tab ).
    ENDLOOP.
  ENDMETHOD.

ENDCLASS.
