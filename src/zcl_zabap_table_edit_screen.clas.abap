CLASS zcl_zabap_table_edit_screen DEFINITION PUBLIC CREATE PUBLIC.

  PUBLIC SECTION.
    CONSTANTS:
      BEGIN OF c_commands,
        ok                  TYPE syst_ucomm  VALUE 'OK',
        back                TYPE syst_ucomm  VALUE 'BACK',
        exit                TYPE syst_ucomm  VALUE 'EXIT',
        cancel              TYPE syst_ucomm  VALUE 'CANCEL',
        toggle_display      TYPE syst_ucomm  VALUE 'TOGGLE_DISPLAY',
        change_document     TYPE syst_ucomm  VALUE 'CHANGE_DOCUMENT',
        save                TYPE syst_ucomm  VALUE 'SAVE',
        validate            TYPE syst_ucomm  VALUE 'VALIDATE',
        reset               TYPE syst_ucomm  VALUE 'RESET',
        restrict_selection  TYPE syst_ucomm  VALUE 'RESTRICT_SELECTION',
        documentation       TYPE syst_ucomm  VALUE 'DOCUMENTATION',
        switch_tech_display TYPE syst_ucomm  VALUE 'SWITCH_TECH_DISPLAY',
      END OF c_commands.

    TYPES:
      BEGIN OF t_config,
        BEGIN OF ext,
          commands TYPE REF TO zif_zabap_table_edit_commands,
        END OF ext,
        disable_cd_view             TYPE abap_bool,
        disable_editing             TYPE abap_bool,
        disable_selection           TYPE abap_bool,
        disable_switch_tech_display TYPE abap_bool,
        BEGIN OF documentation,
          class TYPE doku_class,
          name  TYPE string,
        END OF documentation,
      END OF t_config.

    METHODS:
      constructor IMPORTING config TYPE t_config,
      update_screen_controls IMPORTING in_edit_mode TYPE abap_bool DEFAULT abap_false RAISING zcx_zabap_table_edit .

    DATA:
        config TYPE t_config.
ENDCLASS.



CLASS ZCL_ZABAP_TABLE_EDIT_SCREEN IMPLEMENTATION.


  METHOD constructor.
    me->config = config.
  ENDMETHOD.


  METHOD update_screen_controls.
    zcl_zabap_screen_with_containe=>dynamic_commands->remove_all_commands( ).

    DATA(include_commands) = VALUE ztt_zabap_commands( ( c_commands-ok ) ( c_commands-back )  ( c_commands-exit )  ( c_commands-cancel ) ).

    IF config-disable_editing = abap_false.
      zcl_zabap_screen_with_containe=>dynamic_commands->add_command( command = c_commands-toggle_display
          description = VALUE #( text = TEXT-001 icon_id = '@3I@' icon_text = TEXT-001 ) ).
    ENDIF.

    IF config-disable_cd_view = abap_false.
      zcl_zabap_screen_with_containe=>dynamic_commands->add_command( command = c_commands-change_document
          description = VALUE #( text = TEXT-004 icon_id = '@46@' icon_text = TEXT-004 ) ).
    ENDIF.

    IF config-disable_selection = abap_false.
      zcl_zabap_screen_with_containe=>dynamic_commands->add_command( command = c_commands-restrict_selection
          description = VALUE #( text = TEXT-005 icon_id = '@KG@' icon_text = TEXT-005 quickinfo = TEXT-006 ) ).
    ENDIF.

    IF config-documentation IS NOT INITIAL.
      zcl_zabap_screen_with_containe=>dynamic_commands->add_command( command = c_commands-documentation
          description = VALUE #( text = TEXT-007 icon_id = '@0S@' icon_text = TEXT-007 quickinfo = TEXT-007 ) ).
    ENDIF.

    IF config-disable_switch_tech_display = abap_false.
      zcl_zabap_screen_with_containe=>dynamic_commands->add_command( command = c_commands-switch_tech_display
          description = VALUE #( text = TEXT-008 icon_id = '@9N@' icon_text = TEXT-008 quickinfo = TEXT-008 ) ).
    ENDIF.

    IF in_edit_mode = abap_true AND config-disable_editing = abap_false.
      APPEND c_commands-save TO include_commands.

      zcl_zabap_screen_with_containe=>dynamic_commands->add_command( command = c_commands-validate
          description = VALUE #( text = TEXT-002 icon_id = '@38@' icon_text = TEXT-002 ) ).
      zcl_zabap_screen_with_containe=>dynamic_commands->add_command( command = c_commands-reset
          description = VALUE #( text = TEXT-003 icon_id = '@42@' icon_text = TEXT-003 ) ).
    ENDIF.

    "---EXTENSION CALL---
    config-ext-commands->change_commands( EXPORTING in_edit_mode = in_edit_mode CHANGING commands = include_commands ).

    zcl_zabap_screen_with_containe=>top_commands->include_only_commands( include_commands ).
  ENDMETHOD.
ENDCLASS.
