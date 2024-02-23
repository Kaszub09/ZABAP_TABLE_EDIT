FUNCTION ZABAP_SCREEN_DISPLAY.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     REFERENCE(HEADER_TEXT) TYPE  STRING OPTIONAL
*"     REFERENCE(TOP_COMMANDS_TO_EXCLUDE) TYPE  ZTT_ZABAP_COMMANDS
*"       OPTIONAL
*"     REFERENCE(DYNAMIC_COMMANDS) TYPE  ZTT_ZABAP_SWC_DYNAMIC_COMMAND
*"       OPTIONAL
*"----------------------------------------------------------------------
  " You can use the template 'functionModuleParameter' to add here the signature!
  .
  DATA(screen) = 101. "Screen with no toolbar

  header_text_to_display = header_text.
  commands_to_exclude_from_top = top_commands_to_exclude.

  IF lines( dynamic_commands ) > 0.
    "Map to dynamic commands inside function module
    CLEAR dynamic.
    LOOP AT dynamic_commands REFERENCE INTO DATA(command).
      ASSIGN COMPONENT |DYNAMIC_{ command->position }| OF STRUCTURE dynamic TO FIELD-SYMBOL(<dynamic>).
      <dynamic> = command->description.
    ENDLOOP.

    screen = 100.  "Screen with toolbar
  ENDIF.

  IF was_screen_called = abap_true.
    LEAVE TO SCREEN screen.    "To avoid stacking screens, since call is blocking
  ELSE.
    CALL SCREEN screen.
  ENDIF.
ENDFUNCTION.
