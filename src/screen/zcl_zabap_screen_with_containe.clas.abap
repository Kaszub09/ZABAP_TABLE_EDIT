CLASS zcl_zabap_screen_with_containe DEFINITION PUBLIC FINAL CREATE PRIVATE.

  PUBLIC SECTION.
    CLASS-EVENTS:
      on_user_command EXPORTING VALUE(command) LIKE sy-ucomm.

    CLASS-METHODS:
      class_constructor,
      "! <p class="shorttext synchronized">Get container to fill with data before displaying it</p>
      get_container RETURNING VALUE(container) TYPE REF TO cl_gui_container,
      "! <p class="shorttext synchronized">Call LEAVE TO SCREEN 0. to return to previous screen.</p>
      display IMPORTING header_text TYPE string OPTIONAL,
      "! <p class="shorttext synchronized">ZABAP_SCREEN_WITH_CONTAINE internal use only!</p>
      raise_on_user_command_event IMPORTING command LIKE sy-ucomm.

    CLASS-DATA:
      top_commands     TYPE REF TO zcl_zabap_swc_top_commands  READ-ONLY,
      dynamic_commands TYPE REF TO zcl_zabap_swc_dynamic_commands READ-ONLY.
ENDCLASS.

CLASS zcl_zabap_screen_with_containe IMPLEMENTATION.
  METHOD class_constructor.
    top_commands = NEW #( ).
    dynamic_commands = NEW #( ).
  ENDMETHOD.

  METHOD get_container.
    CALL FUNCTION 'ZABAP_SCREEN_GET_CONTAINER' IMPORTING container = container.
  ENDMETHOD.

  METHOD display.
    CALL FUNCTION 'ZABAP_SCREEN_DISPLAY'
      EXPORTING
        header_text             = header_text
        top_commands_to_exclude = top_commands->commands_to_exclude
        dynamic_commands        = dynamic_commands->get_commands( ).
  ENDMETHOD.

  METHOD raise_on_user_command_event.
    DATA(command_converted) = command.
    IF command CP 'DYNAMIC_*'.
      DATA(added_dynamic_commands) = dynamic_commands->get_commands( ).
      command_converted = added_dynamic_commands[ position = CONV i( replace( val = command_converted sub = 'DYNAMIC_' with = || ) ) ]-command.
    ENDIF.

    RAISE EVENT on_user_command EXPORTING command = command_converted.
  ENDMETHOD.
ENDCLASS.
