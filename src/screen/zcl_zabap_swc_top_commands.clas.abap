CLASS zcl_zabap_swc_top_commands DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    CLASS-METHODS:
      class_constructor.

    METHODS:
      "! <p class="shorttext synchronized">OK, BACK, EXIT, CANCEL</p>
      set_navigation_only_commands,
      exclude_only_commands IMPORTING functions TYPE ztt_zabap_commands,
      include_only_commands IMPORTING functions TYPE ztt_zabap_commands.

    CLASS-DATA:
        all_available_commands TYPE ztt_zabap_commands READ-ONLY .

    DATA:
        commands_to_exclude TYPE ztt_zabap_commands READ-ONLY.
  PROTECTED SECTION.
  PRIVATE SECTION.

ENDCLASS.


CLASS zcl_zabap_swc_top_commands IMPLEMENTATION.
  METHOD  class_constructor.
    all_available_commands = VALUE #( ( |OK| ) ( |SAVE| )  ( |BACK| )  ( |EXIT| )  ( |CANCEL| ) ( |PRINT| )
                                       ( |SEARCH| ) ( |SEARCH+| ) ( |P--| ) ( |P-| ) ( |P+| ) ( |P++| ) ).
  ENDMETHOD.

  METHOD set_navigation_only_commands.
    include_only_commands( value #( ( |OK| ) ( |BACK| )  ( |EXIT| )  ( |CANCEL| ) ) ).
  ENDMETHOD.

  METHOD exclude_only_commands.
    commands_to_exclude = functions.
  ENDMETHOD.

  METHOD include_only_commands.
    CLEAR commands_to_exclude.
    LOOP AT all_available_commands REFERENCE INTO DATA(available_command).
      IF NOT line_exists( functions[ table_line = available_command->* ] ).
        APPEND available_command->* TO commands_to_exclude.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.


ENDCLASS.
