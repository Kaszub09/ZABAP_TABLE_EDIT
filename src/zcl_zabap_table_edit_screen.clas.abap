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
          commands TYPE STANDARD TABLE OF REF TO zif_zabap_table_edit_commands WITH EMPTY KEY,
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



CLASS zcl_zabap_table_edit_screen IMPLEMENTATION.


  METHOD constructor.
    me->config = config.
  ENDMETHOD.


  METHOD update_screen_controls.
    zcl_zabap_screen_with_containe=>dynamic_commands->remove_all_commands( ).
    DATA(top_commands) = VALUE ztt_zabap_commands( ( c_commands-ok ) ( c_commands-back )  ( c_commands-exit )  ( c_commands-cancel ) ).
    DATA(commands) = VALUE zif_zabap_table_edit_commands=>tt_command( ).

    IF config-disable_editing = abap_false.
      APPEND VALUE #( command = c_commands-toggle_display
           description = VALUE #( text = TEXT-001 icon_id = '@3I@' icon_text = TEXT-001 ) ) TO commands.
    ENDIF.

    IF config-disable_cd_view = abap_false.
      APPEND VALUE #( command = c_commands-change_document
         description = VALUE #( text = TEXT-004 icon_id = '@46@' icon_text = TEXT-004 ) ) TO commands.
    ENDIF.

    IF config-disable_selection = abap_false.
      APPEND VALUE #( command = c_commands-restrict_selection
         description = VALUE #( text = TEXT-005 icon_id = '@KG@' icon_text = TEXT-005 quickinfo = TEXT-006 ) ) TO commands.
    ENDIF.

    IF config-documentation IS NOT INITIAL.
      APPEND VALUE #( command = c_commands-documentation
         description = VALUE #( text = TEXT-007 icon_id = '@0S@' icon_text = TEXT-007 quickinfo = TEXT-007 ) ) TO commands.
    ENDIF.

    IF config-disable_switch_tech_display = abap_false.
      APPEND VALUE #( command = c_commands-switch_tech_display
         description = VALUE #( text = TEXT-008 icon_id = '@9N@' icon_text = TEXT-008 quickinfo = TEXT-008 ) ) TO commands.
    ENDIF.

    IF in_edit_mode = abap_true AND config-disable_editing = abap_false.
      APPEND c_commands-save TO top_commands.

      APPEND VALUE #( command = c_commands-validate
          description = VALUE #( text = TEXT-002 icon_id = '@38@' icon_text = TEXT-002 ) ) TO commands.
      APPEND VALUE #( command = c_commands-reset
         description = VALUE #( text = TEXT-003 icon_id = '@42@' icon_text = TEXT-003 ) ) TO commands.
    ENDIF.

    "---EXTENSION CALL---
    LOOP AT config-ext-commands INTO DATA(ext).
      ext->change_commands( EXPORTING in_edit_mode = in_edit_mode CHANGING top_commands = top_commands commands = commands ).
    ENDLOOP.

    LOOP AT commands REFERENCE INTO DATA(cmd).
      zcl_zabap_screen_with_containe=>dynamic_commands->add_command( command = cmd->command description = cmd->description ).
    ENDLOOP.
    zcl_zabap_screen_with_containe=>top_commands->include_only_commands( top_commands ).
  ENDMETHOD.
ENDCLASS.
