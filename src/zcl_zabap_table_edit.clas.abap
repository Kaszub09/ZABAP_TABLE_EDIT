CLASS zcl_zabap_table_edit DEFINITION PUBLIC FINAL CREATE PUBLIC.

  PUBLIC SECTION.
    TYPES:
      BEGIN OF t_config,
        display_text       TYPE string,
        table_name         TYPE  string, "TODO tabname
        change_doc_type    TYPE zabap_change_doc_type,
        disable_cd_view    TYPE abap_bool,
        disable_editing    TYPE abap_bool,
        disable_text_table TYPE abap_bool,
        BEGIN OF ext,
          commands TYPE REF TO zif_zabap_table_edit_commands,
          config   TYPE REF TO zif_zabap_table_edit_config,
          data     TYPE REF TO zif_zabap_table_edit_data,
        END OF ext,
      END OF t_config.

    METHODS:
      constructor IMPORTING configuration TYPE t_config,
      set_edit_mode IMPORTING editable TYPE abap_bool RETURNING VALUE(success) TYPE abap_bool,
      display.

  PRIVATE SECTION.
    METHODS:
      initialize_extensions,
      command_validate,
      command_save,
      command_toggle_display,
      commad_change_document,
      command_cancel,
      command_exit,
      command_reset.

    METHODS:
      on_user_command FOR EVENT on_user_command OF zcl_zabap_screen_with_containe IMPORTING command.

    DATA:
      config     TYPE t_config.

    DATA:
      in_edit_mode    TYPE abap_bool VALUE abap_false,
      messages        TYPE REF TO zcl_zabap_table_edit_messages,
      screen_controls TYPE REF TO zcl_zabap_table_edit_screen,
      table_data      TYPE REF TO zcl_zabap_table_edit_tab_data.
ENDCLASS.


CLASS zcl_zabap_table_edit IMPLEMENTATION.
  METHOD constructor.
    config = configuration.
    initialize_extensions( ).
    "---EXTENSION CALL---
    config-ext-config->change_config( CHANGING config = config ).

    messages = NEW #( ). "TODO as interface
    screen_controls = NEW #( CORRESPONDING #( config ) ). "TODO as interface

    SET HANDLER me->on_user_command.

    table_data = NEW #( CORRESPONDING #( config ) ).
  ENDMETHOD.

  METHOD initialize_extensions.
    DATA(empty_extension) = NEW zcl_zabap_table_edit_empty_ext( ).
    IF NOT config-ext-commands IS BOUND.
      config-ext-commands = empty_extension.
    ENDIF.
    IF NOT config-ext-data IS BOUND.
      config-ext-data = empty_extension.
    ENDIF.
    IF NOT config-ext-config IS BOUND.
      config-ext-config = empty_extension.
    ENDIF.
  ENDMETHOD.

  METHOD set_edit_mode.
    DATA(new_edit_mode) = COND #( WHEN config-disable_editing = abap_true THEN abap_false ELSE editable ).
    "---EXTENSION CALL---
    config-ext-commands->set_edit_mode( CHANGING editable = new_edit_mode ).

    "Unlock table first in case of errors or some remaining locks
    table_data->unlock_table( ).

    IF new_edit_mode = abap_true.
      DATA msg TYPE string.
      IF table_data->lock_table( IMPORTING error_message = msg ) = abap_false.
        new_edit_mode = abap_false.
        messages->display_error( msg ).
      ENDIF.
    ENDIF.

    in_edit_mode = new_edit_mode.
    success = xsdbool( in_edit_mode = editable ).
  ENDMETHOD.

  METHOD display.
    screen_controls->update_screen_controls( in_edit_mode ).
    table_data->reset_grid( in_edit_mode ).
    "---EXTENSION CALL---
    config-ext-data->change_display_text( CHANGING display_text = config-display_text ).
    zcl_zabap_screen_with_containe=>display( header_text = config-display_text ).
  ENDMETHOD.

  METHOD on_user_command.
    DATA(cancel_command) = abap_false.
    "---EXTENSION CALL---
    config-ext-commands->before_command( CHANGING command = command cancel_command = cancel_command ).

    IF cancel_command = abap_true.
      RETURN.
    ENDIF.

    CASE command.
      WHEN 'SAVE'. command_save( ).
      WHEN 'TOGGLE_DISPLAY'. command_toggle_display( ).
      WHEN 'VALIDATE'. command_validate( ).
      WHEN 'CHANGE_DOCUMENT'. commad_change_document( ).
      WHEN 'RESET'. command_reset( ).
      WHEN 'BACK' OR 'EXIT'. command_exit( ).
      WHEN 'CANCEL'. command_cancel( ).
    ENDCASE.

    "---EXTENSION CALL---
    config-ext-commands->after_command( CHANGING command = command ).
  ENDMETHOD.

  METHOD command_save.
    table_data->validate( IMPORTING result = DATA(result) compared = DATA(compared) ).

    CASE result.
      WHEN zcl_zabap_table_edit_globals=>c_validation-incorrect_values OR zcl_zabap_table_edit_globals=>c_validation-extension_invalid.
        "^Nothing to do - message was already displayed by ALV GRID or by extension
        RETURN.
      WHEN zcl_zabap_table_edit_globals=>c_validation-duplicates.
        messages->show_duplicates( table_name = config-table_name duplicates = compared-duplicates mandant_col_name = table_data->mandant_field ).
        RETURN.
      WHEN zcl_zabap_table_edit_globals=>c_validation-ok.
      WHEN OTHERS.
        messages->unexpected_validation_result( ).
        RETURN.
    ENDCASE.

    IF messages->confirm_save( ) = abap_false.
      RETURN.
    ENDIF.

    DATA msg TYPE string.
    IF table_data->save_data( IMPORTING erorr_message = msg CHANGING compared = compared ) = abap_true.
      messages->save_ok( ).
      set_edit_mode( abap_false ).
      table_data->reset_grid( in_edit_mode ).
      display( ).
    ELSE.
      messages->display_error( msg ).
    ENDIF.

  ENDMETHOD.

  METHOD command_toggle_display.
    IF in_edit_mode = abap_true.
      IF messages->confirm_data_loss( table_data->was_data_changed ) = abap_false.
        RETURN.
      ENDIF.
    ENDIF.
    set_edit_mode( COND #( WHEN in_edit_mode = abap_true THEN abap_false ELSE abap_true ) ).
    display( ).
  ENDMETHOD.

  METHOD command_validate.
    table_data->validate( IMPORTING result = DATA(result) compared = DATA(compared) ).
    CASE result.
      WHEN zcl_zabap_table_edit_globals=>c_validation-incorrect_values OR zcl_zabap_table_edit_globals=>c_validation-extension_invalid.
        "^Nothing to do - message was already displayed by ALV GRID or by extension
      WHEN zcl_zabap_table_edit_globals=>c_validation-duplicates.
        messages->show_duplicates( table_name = config-table_name duplicates = compared-duplicates mandant_col_name = table_data->mandant_field ).
      WHEN zcl_zabap_table_edit_globals=>c_validation-ok.
        messages->validation_ok( ).
      WHEN OTHERS.
        messages->unexpected_validation_result( ).
    ENDCASE.
  ENDMETHOD.

  METHOD commad_change_document.
    DATA batch_input TYPE TABLE OF bdcdata.

    APPEND VALUE #( program = 'RSSCD100' dynpro = '1000' dynbegin = 'X' fnam = 'BDC_CURSOR' fval = 'TABNAME'  ) TO batch_input.
    APPEND VALUE #( fnam = 'OBJEKT' fval = '' ) TO batch_input.
    APPEND VALUE #( fnam = 'OBJEKTID' fval = config-table_name ) TO batch_input.
    APPEND VALUE #( fnam = 'TABNAME' fval = config-table_name ) TO batch_input.

    "Clear in case of leftovers from previous call
    APPEND VALUE #( fnam = 'TABKEY' fval = '' ) TO batch_input.
    APPEND VALUE #( fnam = 'TABKEYLO' fval = '' ) TO batch_input.

    DATA(selected_row_key) = table_data->get_selected_row_key( ).
    IF strlen( selected_row_key ) <= 70.
      APPEND VALUE #( fnam = 'TABKEY' fval = selected_row_key ) TO batch_input.
    ELSE.
      APPEND VALUE #( fnam = 'TABKEYLO' fval = selected_row_key ) TO batch_input.
    ENDIF.

    CALL TRANSACTION 'RSSCD100' USING batch_input MODE 'E' UPDATE 'S'.
  ENDMETHOD.

  METHOD command_cancel.
    IF messages->confirm_data_loss( table_data->was_data_changed ) = abap_false.
      RETURN.
    ENDIF.
    LEAVE PROGRAM.
  ENDMETHOD.

  METHOD command_exit.
    IF messages->confirm_data_loss( table_data->was_data_changed ) = abap_false.
      RETURN.
    ENDIF.
    LEAVE TO SCREEN 0.
  ENDMETHOD.

  METHOD command_reset.
    IF messages->confirm_data_loss( table_data->was_data_changed ) = abap_false.
      RETURN.
    ENDIF.
    table_data->reset_grid( in_edit_mode ).
  ENDMETHOD.
ENDCLASS.
