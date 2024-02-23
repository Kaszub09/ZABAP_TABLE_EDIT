CLASS zcl_zabap_table_edit_if_chain DEFINITION
  PUBLIC
  CREATE PUBLIC .

  PUBLIC SECTION.
    INTERFACES:
      zif_zabap_table_edit .

  PROTECTED SECTION.
    METHODS:
      add_interface IMPORTING interface TYPE REF TO zif_zabap_table_edit.
  PRIVATE SECTION.
    DATA:
      interfaces TYPE STANDARD TABLE OF REF TO zif_zabap_table_edit.
ENDCLASS.



CLASS zcl_zabap_table_edit_if_chain IMPLEMENTATION.


  METHOD add_interface.
    APPEND interface TO interfaces.
  ENDMETHOD.


  METHOD zif_zabap_table_edit~additional_fields.
    LOOP AT interfaces REFERENCE INTO DATA(interface).
      interface->*->additional_fields( CHANGING additional_fields = additional_fields ).
    ENDLOOP.
  ENDMETHOD.


  METHOD zif_zabap_table_edit~additional_validation.
    LOOP AT interfaces REFERENCE INTO DATA(interface).
      interface->*->additional_validation( CHANGING result = result  all_modified_data  =  all_modified_data  duplicates = duplicates
                                         inserted = inserted deleted = deleted before_modified = before_modified modified = modified ).
    ENDLOOP.
  ENDMETHOD.


  METHOD zif_zabap_table_edit~after_command.
    LOOP AT interfaces REFERENCE INTO DATA(interface).
      interface->*->after_command( CHANGING command  = command  ).
    ENDLOOP.
  ENDMETHOD.


  METHOD zif_zabap_table_edit~after_save.
    LOOP AT interfaces REFERENCE INTO DATA(interface).
      interface->*->after_save( CHANGING inserted = inserted deleted = deleted before_modified = before_modified modified = modified ).
    ENDLOOP.
  ENDMETHOD.


  METHOD zif_zabap_table_edit~before_command.
    LOOP AT interfaces REFERENCE INTO DATA(interface).
      interface->*->before_command( CHANGING command = command cancel_command = cancel_command ).
    ENDLOOP.
  ENDMETHOD.


  METHOD zif_zabap_table_edit~before_save.
    LOOP AT interfaces REFERENCE INTO DATA(interface).
      interface->*->before_save( CHANGING inserted = inserted deleted = deleted before_modified = before_modified modified = modified ).
    ENDLOOP.
  ENDMETHOD.


  METHOD zif_zabap_table_edit~change_commands.
    LOOP AT interfaces REFERENCE INTO DATA(interface).
      interface->*->change_commands(  in_edit_mode  =  in_edit_mode  ).
    ENDLOOP.
  ENDMETHOD.


  METHOD zif_zabap_table_edit~grid_setup.
    LOOP AT interfaces REFERENCE INTO DATA(interface).
      interface->*->grid_setup( CHANGING grid    = grid    ).
    ENDLOOP.
  ENDMETHOD.


  METHOD zif_zabap_table_edit~initial_data.
    LOOP AT interfaces REFERENCE INTO DATA(interface).
      interface->*->initial_data( CHANGING initial_data = initial_data ).
    ENDLOOP.
  ENDMETHOD.


  METHOD zif_zabap_table_edit~on_data_changed.
    LOOP AT interfaces REFERENCE INTO DATA(interface).
      interface->*->on_data_changed( er_data_changed = er_data_changed e_onf4 = e_onf4 e_onf4_before = e_onf4_before
        e_onf4_after = e_onf4_after e_ucomm = e_ucomm sender = sender ).
    ENDLOOP.
  ENDMETHOD.


  METHOD zif_zabap_table_edit~on_data_changed_finished.
    LOOP AT interfaces REFERENCE INTO DATA(interface).
      interface->*->on_data_changed_finished( e_modified = e_modified et_good_cells = et_good_cells sender = sender ).
    ENDLOOP.
  ENDMETHOD.


  METHOD zif_zabap_table_edit~refresh_grid.
    LOOP AT interfaces REFERENCE INTO DATA(interface).
      interface->*->refresh_grid( EXPORTING in_edit_mode = in_edit_mode CHANGING field_catalogue = field_catalogue header_text = header_text
                                    initial_data = initial_data modified_data_ext = modified_data_ext ).
    ENDLOOP.
  ENDMETHOD.


  METHOD zif_zabap_table_edit~set_edit_mode.
    LOOP AT interfaces REFERENCE INTO DATA(interface).
      interface->*->set_edit_mode( CHANGING editable = editable ).
    ENDLOOP.
  ENDMETHOD.
ENDCLASS.
