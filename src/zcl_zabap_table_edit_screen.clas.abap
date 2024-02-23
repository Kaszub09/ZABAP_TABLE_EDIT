CLASS zcl_zabap_table_edit_screen DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    METHODS:
      update_screen_controls IMPORTING editable TYPE abap_bool DEFAULT abap_false.

ENDCLASS.

CLASS zcl_zabap_table_edit_screen IMPLEMENTATION.
  METHOD update_screen_controls.
    zcl_zabap_screen_with_containe=>dynamic_commands->remove_all_commands( ).

    DATA(include_commands) = VALUE ztt_zabap_commands( ( |OK| )  ( |BACK| )  ( |EXIT| )  ( |CANCEL| ) ).

    zcl_zabap_screen_with_containe=>dynamic_commands->add_command( command = 'TOGGLE_DISPLAY'
        description = VALUE #( text = TEXT-001 icon_id = '@3I@' icon_text = TEXT-001 ) ).
    zcl_zabap_screen_with_containe=>dynamic_commands->add_command( command = 'CHANGE_DOCUMENT'
        description = VALUE #( text = TEXT-004 icon_id = '@46@' icon_text = TEXT-004 ) ).

    IF editable = abap_true.
      APPEND |SAVE| TO include_commands.

      zcl_zabap_screen_with_containe=>dynamic_commands->add_command( command = 'VALIDATE'
          description = VALUE #( text = TEXT-002 icon_id = '@38@' icon_text = TEXT-002 ) ).
      zcl_zabap_screen_with_containe=>dynamic_commands->add_command( command = 'RESET'
          description = VALUE #( text = TEXT-003 icon_id = '@42@' icon_text = TEXT-003 ) ).
    ENDIF.

    zcl_zabap_screen_with_containe=>top_commands->include_only_commands( include_commands ).
  ENDMETHOD.
ENDCLASS.
