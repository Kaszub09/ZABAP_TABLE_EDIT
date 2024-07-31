"! <p class="shorttext synchronized">Additional validation whether selected fields were filled</p>
"!
CLASS zcl_zabap_table_edit_fill_ext DEFINITION PUBLIC FINAL CREATE PUBLIC.

  PUBLIC SECTION.
    INTERFACES:
      zif_zabap_table_edit_commands,
      zif_zabap_table_edit_config,
      zif_zabap_table_edit_data.

    TYPES:
      tt_fields TYPE RANGE OF fieldname.

    METHODS:
      "! @parameter fields_restriction | <p class="shorttext synchronized" lang="en">Fields that must be non-initial. MANDT is always included...</p>
      "! Should be added manually if has another name
      constructor IMPORTING fields_restriction TYPE tt_fields OPTIONAL.

  PRIVATE SECTION.
    DATA:
      components         TYPE cl_abap_structdescr=>component_table,
      fields_restriction TYPE tt_fields.
ENDCLASS.

CLASS zcl_zabap_table_edit_fill_ext IMPLEMENTATION.
  METHOD constructor.
    me->fields_restriction = fields_restriction.
    APPEND VALUE #( sign = 'E' option = 'EQ' low = 'MANDT' ) TO me->fields_restriction.
  ENDMETHOD.

  METHOD zif_zabap_table_edit_data~additional_fields.
  ENDMETHOD.

  METHOD zif_zabap_table_edit_data~additional_validation.
    FIELD-SYMBOLS <table> TYPE table.

    ASSIGN all_modified_data->* TO <table>.

    LOOP AT components REFERENCE INTO DATA(component).
      LOOP AT <table> ASSIGNING FIELD-SYMBOL(<row>).
        DATA(row_index) = sy-tabix.
        ASSIGN COMPONENT component->name OF STRUCTURE <row> TO FIELD-SYMBOL(<component_value>).
        IF <component_value> IS INITIAL.
          DATA(elemdescr) = CAST cl_abap_elemdescr( component->type ).
          DATA(ddic_field) = elemdescr->get_ddic_field( ).
          IF ddic_field-domname <> 'XFELD' AND ddic_field-domname <> 'XFIELD'. "Skip true/false checkboxes
            MESSAGE e017(zabap_table_edit) WITH ddic_field-reptext row_index.
          ENDIF.
        ENDIF.
      ENDLOOP.
    ENDLOOP.
  ENDMETHOD.

  METHOD zif_zabap_table_edit_commands~after_command.
  ENDMETHOD.

  METHOD zif_zabap_table_edit_data~after_save.
  ENDMETHOD.

  METHOD zif_zabap_table_edit_commands~before_command.
  ENDMETHOD.

  METHOD zif_zabap_table_edit_data~before_save.
  ENDMETHOD.

  METHOD zif_zabap_table_edit_commands~change_commands.
  ENDMETHOD.

  METHOD zif_zabap_table_edit_config~change_config.
  ENDMETHOD.

  METHOD zif_zabap_table_edit_data~default_select.
  ENDMETHOD.

  METHOD zif_zabap_table_edit_config~grid_setup.
  ENDMETHOD.

  METHOD zif_zabap_table_edit_data~initial_data.
    DATA(table_descr) = CAST cl_abap_tabledescr( cl_abap_structdescr=>describe_by_data_ref( initial_data ) ).
    DATA(line_descr) = CAST cl_abap_structdescr( table_descr->get_table_line_type( ) ).
    components = line_descr->get_components( ).
    DELETE components WHERE NOT name IN fields_restriction.
  ENDMETHOD.

  METHOD zif_zabap_table_edit_data~on_data_changed.
  ENDMETHOD.

  METHOD zif_zabap_table_edit_data~on_data_changed_finished.
  ENDMETHOD.

  METHOD zif_zabap_table_edit_data~refresh_grid.
  ENDMETHOD.

  METHOD zif_zabap_table_edit_commands~set_edit_mode.
  ENDMETHOD.

  METHOD zif_zabap_table_edit_data~change_display_text.
  ENDMETHOD.
  METHOD zif_zabap_table_edit_config~change_init_selection_fields.

  ENDMETHOD.

ENDCLASS.
