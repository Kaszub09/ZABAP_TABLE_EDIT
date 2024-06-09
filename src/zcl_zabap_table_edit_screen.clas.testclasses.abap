CLASS tcl_zabap_table_edit_screen DEFINITION DEFERRED.
CLASS zcl_zabap_table_edit_screen DEFINITION LOCAL FRIENDS tcl_zabap_table_edit_screen.
CLASS tcl_zabap_table_edit_screen DEFINITION FINAL FOR TESTING RISK LEVEL HARMLESS DURATION SHORT.

  PRIVATE SECTION.
    METHODS:
      setup,
      disable_edit         FOR TESTING,
      disable_cd_view      FOR TESTING,
      edit_mode            FOR TESTING,
      non_edit_mode        FOR TESTING,
      top_command_disabled IMPORTING command TYPE syst_ucomm,
      command_disabled IMPORTING command TYPE syst_ucomm,
      top_command_enabled IMPORTING command TYPE syst_ucomm,
      command_enabled IMPORTING command TYPE syst_ucomm.

    DATA:
        cut TYPE REF TO zcl_zabap_table_edit_screen.
ENDCLASS.

CLASS tcl_zabap_table_edit_screen IMPLEMENTATION.
  METHOD setup.
    cut = NEW #( VALUE #( ext-commands = NEW zcl_zabap_table_edit_empty_ext( ) ) ).
  ENDMETHOD.

  METHOD disable_cd_view.
    cut = NEW #( VALUE #( disable_cd_view = abap_true ext-commands = NEW zcl_zabap_table_edit_empty_ext( ) ) ).
    cut->update_screen_controls( ).
    command_disabled( cut->c_commands-change_document ).
    "Check edit mode
    cut->update_screen_controls( abap_true ).
    command_disabled( cut->c_commands-change_document ).
  ENDMETHOD.

  METHOD disable_edit.
    cut = NEW #( VALUE #( disable_editing = abap_true ext-commands = NEW zcl_zabap_table_edit_empty_ext( ) ) ).
    cut->update_screen_controls( ).
    command_disabled( cut->c_commands-toggle_display ).
    top_command_disabled( cut->c_commands-save ).

    "Check edit mode, just in case
    cut->update_screen_controls( abap_true ).
    command_disabled( cut->c_commands-toggle_display ).
    top_command_disabled( cut->c_commands-save ).
  ENDMETHOD.

  METHOD edit_mode.
    cut->update_screen_controls( abap_true ).
    top_command_enabled( cut->c_commands-save ).
    command_enabled( cut->c_commands-validate ).
    command_enabled( cut->c_commands-reset ).
    command_enabled( cut->c_commands-toggle_display ).
  ENDMETHOD.

  METHOD non_edit_mode.
    cut->update_screen_controls( ).
    top_command_disabled( cut->c_commands-save ).
    command_disabled( cut->c_commands-validate ).
    command_disabled( cut->c_commands-reset ).
    command_enabled( cut->c_commands-toggle_display ).
  ENDMETHOD.

  METHOD command_disabled.
    DATA(commands) = zcl_zabap_screen_with_containe=>dynamic_commands->get_commands( ).
    cl_abap_unit_assert=>assert_false( xsdbool( line_exists( commands[ KEY command command = command ] ) ) ).
  ENDMETHOD.

  METHOD top_command_disabled.
    DATA(excluded_top_commands) = zcl_zabap_screen_with_containe=>top_commands->commands_to_exclude.
    cl_abap_unit_assert=>assert_true( xsdbool( line_exists( excluded_top_commands[ table_line = command ] ) ) ).
  ENDMETHOD.

  METHOD command_enabled.
    DATA(commands) = zcl_zabap_screen_with_containe=>dynamic_commands->get_commands( ).
    cl_abap_unit_assert=>assert_true( xsdbool( line_exists( commands[ KEY command command = command ] ) ) ).
  ENDMETHOD.

  METHOD top_command_enabled.
    DATA(excluded_top_commands) = zcl_zabap_screen_with_containe=>top_commands->commands_to_exclude.
    cl_abap_unit_assert=>assert_false( xsdbool( line_exists( excluded_top_commands[ table_line = command ] ) ) ).
  ENDMETHOD.
ENDCLASS.
