CLASS tcl_zabap_swc_top_commands DEFINITION DEFERRED.
CLASS zcl_zabap_swc_top_commands DEFINITION LOCAL FRIENDS tcl_zabap_swc_top_commands.
CLASS tcl_zabap_swc_top_commands DEFINITION FINAL FOR TESTING DURATION SHORT RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    METHODS:
      setup,
      set_navigation_only_commands FOR TESTING,
      set_exclude_commands FOR TESTING,
      set_include_commands FOR TESTING,
      include_only_existing_comm FOR TESTING.

    DATA:
        cut TYPE REF TO zcl_zabap_swc_top_commands.
ENDCLASS.


CLASS tcl_zabap_swc_top_commands IMPLEMENTATION.
  METHOD setup.
    cut = NEW #( ).
  ENDMETHOD.

  METHOD set_navigation_only_commands.
    cut->set_navigation_only_commands( ).
    cl_abap_unit_assert=>assert_equals( exp = VALUE ztt_zabap_commands( ( |SAVE| ) ( |PRINT| )
                                       ( |SEARCH| ) ( |SEARCH+| ) ( |P--| ) ( |P-| ) ( |P+| ) ( |P++| ) )
                                       act = cut->commands_to_exclude ).
  ENDMETHOD.

  METHOD include_only_existing_comm.
    cut->include_only_commands( VALUE #( ( |DOESNT_EXISTS| ) ) ).
    cl_abap_unit_assert=>assert_equals( exp = cut->all_available_commands act = cut->commands_to_exclude ).
  ENDMETHOD.

  METHOD set_exclude_commands.
    cut->exclude_only_commands( VALUE ztt_zabap_commands( ( |OK| ) ( |BACK| )  ( |EXIT| )  ( |CANCEL| ) ) ).
    cl_abap_unit_assert=>assert_equals( exp = VALUE ztt_zabap_commands( ( |OK| ) ( |BACK| )  ( |EXIT| )  ( |CANCEL| ) )
        act = cut->commands_to_exclude ).
  ENDMETHOD.

  METHOD set_include_commands.
    cut->include_only_commands( VALUE #( ( |SAVE| ) ) ).
    cl_abap_unit_assert=>assert_equals( exp = VALUE ztt_zabap_commands( ( |OK| ) ( |BACK| ) ( |EXIT| ) ( |CANCEL| ) ( |PRINT| )
                                       ( |SEARCH| ) ( |SEARCH+| ) ( |P--| ) ( |P-| ) ( |P+| ) ( |P++| ) )
        act = cut->commands_to_exclude ).
  ENDMETHOD.

ENDCLASS.
