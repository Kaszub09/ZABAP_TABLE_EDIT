CLASS zcl_zabap_swc_dynamic_commands DEFINITION PUBLIC CREATE PUBLIC.

  PUBLIC SECTION.
    CONSTANTS:
        c_max_command_number TYPE i VALUE 35.

    METHODS:
      "! @parameter position | <p class="shorttext synchronized">Position of command on toolbar. 0 - first available...</p>
      "! position, otherwise must be between 1 and C_MAX_COMMAND_NUMBER and not used before.
      "! <br/> Position shortcuts: 1-12 = Ctrl-F1 to Ctrl-F12; 13-24 = Ctrl-Shift-F1 to Ctrl-Shift-F12; 25-35 - available non-standard F and Shift-F
      add_command IMPORTING command TYPE syst_ucomm position TYPE i DEFAULT 0 description TYPE smp_dyntxt RAISING zcx_zabap_table_edit,
      remove_command IMPORTING command TYPE syst_ucomm,
      remove_all_commands,
      get_commands RETURNING VALUE(commands) TYPE ztt_zabap_swc_dynamic_command.

  PRIVATE SECTION.
    DATA: commands TYPE ztt_zabap_swc_dynamic_command.
ENDCLASS.

CLASS zcl_zabap_swc_dynamic_commands IMPLEMENTATION.
  METHOD add_command.
    DATA(pos) = position.
    "Run checks on position
    IF pos < 0 OR pos > c_max_command_number.
      MESSAGE e002(zabap_table_edit) WITH c_max_command_number INTO DATA(pos_invalid).
      RAISE EXCEPTION TYPE zcx_zabap_table_edit EXPORTING custom_message = pos_invalid.
    ENDIF.

    IF pos = 0.
      pos = pos + 1.
      "Find next available position - commands are sorted via position
      LOOP AT commands REFERENCE INTO DATA(command_ref).
        IF command_ref->position <> pos.
          EXIT.
        ENDIF.
        pos = pos + 1.
      ENDLOOP.
      IF pos > c_max_command_number.
        MESSAGE e003(zabap_table_edit) INTO DATA(no_pos_available).
        RAISE EXCEPTION TYPE zcx_zabap_table_edit EXPORTING custom_message = no_pos_available.
      ENDIF.

    ELSE.
      "Check if taken
      IF line_exists( commands[ position = pos ] ).
        MESSAGE e004(zabap_table_edit) WITH pos INTO DATA(pos_taken).
        RAISE EXCEPTION TYPE zcx_zabap_table_edit EXPORTING custom_message = pos_taken.
      ENDIF.
    ENDIF.

    INSERT VALUE #( command = command position = pos description = description ) INTO TABLE commands.
  ENDMETHOD.

  METHOD get_commands.
    commands = me->commands.
  ENDMETHOD.

  METHOD remove_all_commands.
    FREE commands.
  ENDMETHOD.

  METHOD remove_command.
    DELETE commands USING KEY command WHERE command = command.
  ENDMETHOD.
ENDCLASS.
