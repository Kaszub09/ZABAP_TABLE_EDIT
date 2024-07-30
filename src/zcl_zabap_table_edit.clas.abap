CLASS zcl_zabap_table_edit DEFINITION PUBLIC CREATE PUBLIC.

  PUBLIC SECTION.
    TYPES:
      BEGIN OF t_config,
        display_text         TYPE string,
        table_name           TYPE string,
        change_doc_type      TYPE zabap_change_doc_type,
        disable_cd_view      TYPE abap_bool,
        disable_editing      TYPE abap_bool,
        disable_text_table   TYPE abap_bool,
        show_selection_first TYPE abap_bool,
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
      command_validate EXPORTING result TYPE i compared TYPE zcl_zabap_table_edit_globals=>t_data_comparision,
      command_save,
      command_toggle_display,
      command_change_document,
      command_cancel,
      command_exit,
      command_reset,
      command_restrict_selection.

    METHODS:
      on_user_command FOR EVENT on_user_command OF zcl_zabap_screen_with_containe IMPORTING command.

    DATA:
      config             TYPE t_config.

    DATA:
      in_edit_mode    TYPE abap_bool VALUE abap_false,
      messages        TYPE REF TO zcl_zabap_table_edit_messages,
      screen_controls TYPE REF TO zcl_zabap_table_edit_screen,
      table_data      TYPE REF TO zif_zabap_table_edit_tab_data.
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

    table_data = zcl_zabap_table_edit_factory=>get_table_data( config = CORRESPONDING #( config )
        grid = zcl_zabap_table_edit_factory=>get_grid( zcl_zabap_screen_with_containe=>get_container( ) ) ).
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
    zcl_zabap_screen_with_containe=>display( config-display_text ).
  ENDMETHOD.

  METHOD on_user_command.
    DATA(cancel_command) = abap_false.
    "---EXTENSION CALL---
    config-ext-commands->before_command( CHANGING command = command cancel_command = cancel_command ).

    IF cancel_command = abap_true.
      RETURN.
    ENDIF.

    CASE command.
      WHEN screen_controls->c_commands-save. command_save( ).
      WHEN screen_controls->c_commands-toggle_display. command_toggle_display( ).
      WHEN screen_controls->c_commands-validate. command_validate( ).
      WHEN screen_controls->c_commands-change_document. command_change_document( ).
      WHEN screen_controls->c_commands-reset. command_reset( ).
      WHEN screen_controls->c_commands-back OR screen_controls->c_commands-exit. command_exit( ).
      WHEN screen_controls->c_commands-cancel. command_cancel( ).
      WHEN screen_controls->c_commands-restrict_selection. command_restrict_selection( ).
    ENDCASE.

    "---EXTENSION CALL---
    config-ext-commands->after_command( CHANGING command = command ).
  ENDMETHOD.

  METHOD command_save.
    command_validate( IMPORTING result = DATA(result) compared = DATA(compared) ).
    IF result <>  zcl_zabap_table_edit_globals=>c_validation-ok.
      RETURN.
    ENDIF.

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
    table_data->validate( IMPORTING result = result compared = compared ).
    CASE result.
      WHEN zcl_zabap_table_edit_globals=>c_validation-incorrect_values OR zcl_zabap_table_edit_globals=>c_validation-extension_invalid.
        "^Nothing to do - message was already displayed by ALV GRID or by extension
      WHEN zcl_zabap_table_edit_globals=>c_validation-duplicates.
        MESSAGE s007(zabap_table_edit) INTO DATA(duplicates_header).
        messages->show_data( msg = duplicates_header table_name = config-table_name data_table = compared-duplicates
                             mandant_col_name = table_data->mandant_field ).
      WHEN zcl_zabap_table_edit_globals=>c_validation-not_in_selection.
        MESSAGE s016(zabap_table_edit) INTO DATA(not_in_selection_header).
        messages->show_data( msg = not_in_selection_header table_name = config-table_name data_table = compared-not_in_selection
                             mandant_col_name = table_data->mandant_field ).
      WHEN zcl_zabap_table_edit_globals=>c_validation-ok.
        messages->validation_ok( ).
      WHEN OTHERS.
        messages->unexpected_validation_result( ).
    ENDCASE.
  ENDMETHOD.

  METHOD command_change_document.
    DATA batch_input TYPE TABLE OF bdcdata.

    APPEND VALUE #( program = 'RSSCD100' dynpro = '1000' dynbegin = 'X' fnam = 'BDC_CURSOR' fval = 'TABNAME' ) TO batch_input.
    APPEND VALUE #( fnam = 'OBJEKT' fval = '' ) TO batch_input.
    APPEND VALUE #( fnam = 'OBJEKTID' fval = config-table_name ) TO batch_input.
    APPEND VALUE #( fnam = 'TABNAME' fval = config-table_name ) TO batch_input.

    "Clear in case of leftovers from previous call
    APPEND VALUE #( fnam = 'TABKEY' fval = '' ) TO batch_input.
    APPEND VALUE #( fnam = 'TABKEYLO' fval = '' ) TO batch_input.

    DATA(selected_row_key) = table_data->get_selected_row_key( ).
    IF strlen( selected_row_key ) <= 70. "length of tabkey
      APPEND VALUE #( fnam = 'TABKEY' fval = selected_row_key ) TO batch_input.
    ELSE.
      APPEND VALUE #( fnam = 'TABKEYLO' fval = selected_row_key ) TO batch_input.
    ENDIF.

    DATA(call_options) = VALUE ctu_params( dismode = 'E' updmode = 'S' nobinpt = abap_true nobiend = abap_true ).
    CALL TRANSACTION 'RSSCD100' USING batch_input OPTIONS FROM call_options.
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

  METHOD command_restrict_selection.
    IF table_data->restrict_selection( ) = abap_true.
      display( ).
    ENDIF.
  ENDMETHOD.

ENDCLASS.
