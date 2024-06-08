CLASS zcl_zabap_table_edit_screen DEFINITION PUBLIC FINAL CREATE PUBLIC.

  PUBLIC SECTION.
    TYPES:
      BEGIN OF t_config,
        BEGIN OF ext,
          commands TYPE REF TO zif_zabap_table_edit_commands,
        END OF ext,
        disable_cd_view         TYPE abap_bool,
        disable_editing TYPE abap_bool,
      END OF t_config.

    METHODS:
      constructor IMPORTING config TYPE t_config,
      update_screen_controls IMPORTING in_edit_mode TYPE abap_bool DEFAULT abap_false.

    DATA:
        config TYPE t_config.
ENDCLASS.


CLASS zcl_zabap_table_edit_screen IMPLEMENTATION.
  METHOD constructor.
    me->config = config.
  ENDMETHOD.

  METHOD update_screen_controls.
    zcl_zabap_screen_with_containe=>dynamic_commands->remove_all_commands( ).

    DATA(include_commands) = VALUE ztt_zabap_commands( ( |OK| )  ( |BACK| )  ( |EXIT| )  ( |CANCEL| ) ).

    IF config-disable_editing = abap_false.
      zcl_zabap_screen_with_containe=>dynamic_commands->add_command( command = 'TOGGLE_DISPLAY'
          description = VALUE #( text = TEXT-001 icon_id = '@3I@' icon_text = TEXT-001 ) ).
    ENDIF.

    IF config-disable_cd_view = abaP_false.
      zcl_zabap_screen_with_containe=>dynamic_commands->add_command( command = 'CHANGE_DOCUMENT'
          description = VALUE #( text = TEXT-004 icon_id = '@46@' icon_text = TEXT-004 ) ).
    ENDIF.

    IF in_edit_mode = abap_true AND config-disable_editing = abap_false.
      APPEND |SAVE| TO include_commands.

      zcl_zabap_screen_with_containe=>dynamic_commands->add_command( command = 'VALIDATE'
          description = VALUE #( text = TEXT-002 icon_id = '@38@' icon_text = TEXT-002 ) ).
      zcl_zabap_screen_with_containe=>dynamic_commands->add_command( command = 'RESET'
          description = VALUE #( text = TEXT-003 icon_id = '@42@' icon_text = TEXT-003 ) ).
    ENDIF.

    "---EXTENSION CALL---
    config-ext-commands->change_commands( EXPORTING in_edit_mode = in_edit_mode CHANGING commands = include_commands ).

    zcl_zabap_screen_with_containe=>top_commands->include_only_commands( include_commands ).
  ENDMETHOD.
ENDCLASS.
