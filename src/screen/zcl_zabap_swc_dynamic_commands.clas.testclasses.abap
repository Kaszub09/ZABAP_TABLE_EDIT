
CLASS tcl_zabap_swc_dynamic_commands DEFINITION FINAL FOR TESTING DURATION SHORT RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    METHODS:
      setup,
      add_three_commands,
      commands_are_added FOR TESTING,
      commands_are_freed FOR TESTING,
      command_is_removed FOR TESTING,
      cant_add_too_many_commands FOR TESTING,
      cant_add_same_pos_commands FOR TESTING,
      cant_add_non_avail_pos_command FOR TESTING.

    DATA:
        cut TYPE REF TO zcl_zabap_swc_dynamic_commands.
ENDCLASS.


CLASS tcl_zabap_swc_dynamic_commands IMPLEMENTATION.
  METHOD setup.
    cut = NEW #( ).
  ENDMETHOD.



  METHOD add_three_commands.
    cut->add_command( command = 'COMMAND1' description = VALUE #( ) ).
    cut->add_command( command = 'COMMAND2' description = VALUE #( ) position = 10 ).
    cut->add_command( command = 'COMMAND3' description = VALUE #( ) ).
  ENDMETHOD.

  METHOD commands_are_added.
    add_three_commands( ).

    DATA(commands) = cut->get_commands( ).
    cl_abap_unit_assert=>assert_equals( exp = 3 act = lines( commands ) ).
    cl_abap_unit_assert=>assert_true( xsdbool( line_exists( commands[ KEY command command = 'COMMAND1' ] ) ) ).
    cl_abap_unit_assert=>assert_true( xsdbool( line_exists( commands[ KEY command command = 'COMMAND2' ] ) ) ).
    cl_abap_unit_assert=>assert_true( xsdbool( line_exists( commands[ KEY command command = 'COMMAND3' ] ) ) ).
  ENDMETHOD.

  METHOD commands_are_freed.
    add_three_commands( ).
    cut->remove_all_commands( ).
    DATA(commands) = cut->get_commands( ).
    cl_abap_unit_assert=>assert_equals( exp = 0 act = lines( commands ) ).
  ENDMETHOD.

  METHOD command_is_removed.
    add_three_commands( ).
    cut->remove_command( 'COMMAND1' ).

    DATA(commands) = cut->get_commands( ).
    cl_abap_unit_assert=>assert_equals( exp = 2 act = lines( commands ) ).
    cl_abap_unit_assert=>assert_true( xsdbool( line_exists( commands[ KEY command command = 'COMMAND2' ] ) ) ).
    cl_abap_unit_assert=>assert_true( xsdbool( line_exists( commands[ KEY command command = 'COMMAND3' ] ) ) ).
  ENDMETHOD.

  METHOD cant_add_non_avail_pos_command.
    TRY.
        add_three_commands( ).
        cut->add_command( command = 'COMMAND' description = VALUE #( ) position = -20 ).

        cl_abap_unit_assert=>fail( |Exception not raised| ).
      CATCH zcx_zabap_table_edit.
    ENDTRY.
  ENDMETHOD.


  METHOD cant_add_same_pos_commands.
    TRY.
        add_three_commands( ).
        cut->add_command( command = 'COMMAND' description = VALUE #( ) position = 10 ).

        cl_abap_unit_assert=>fail( |Exception not raised| ).
      CATCH zcx_zabap_table_edit.
    ENDTRY.
  ENDMETHOD.

  METHOD cant_add_too_many_commands.
    TRY.
        add_three_commands( ).
        DO cut->c_max_command_number TIMES.
          cut->add_command( command = 'COMMAND' description = VALUE #( ) ).
        ENDDO.

        cl_abap_unit_assert=>fail( |Exception not raised| ).
      CATCH zcx_zabap_table_edit.
    ENDTRY.
  ENDMETHOD.

ENDCLASS.
