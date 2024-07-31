CLASS zcl_zabap_table_edit_descr_ext DEFINITION PUBLIC FINAL CREATE PUBLIC.

  PUBLIC SECTION.
    INTERFACES:
      zif_zabap_table_edit_commands,
      zif_zabap_table_edit_config,
      zif_zabap_table_edit_data.

    TYPES:
      tt_trigger_change_fields TYPE SORTED TABLE OF fieldname WITH UNIQUE KEY table_line,
      BEGIN OF t_new_fields,
        "! E.g. field name like 'TADIR-OBJECT' or data element like 'TROBJTYPE'
        type_name   TYPE string,
        "! Name both in select clause of query and one visible on grid
        new_name    TYPE fieldname,
        "! Fill to position column after field from base table
        after_field TYPE fieldname,
      END OF t_new_fields,
      tt_new_fields TYPE STANDARD TABLE OF t_new_fields WITH EMPTY KEY,
      BEGIN OF t_query,
        "! Select clause, should use same fields names as new_name in new_fields
        select TYPE string,
        "! Tables to select from, can be joined
        from   TYPE string,
        "! should use '@row-[modified_data_ext field]' (new SQL syntax - base table line is mapped to row)
        where  TYPE string,
      END OF t_query.

    METHODS:
      "! <p class="shorttext synchronized" lang="en"></p>
      "! @parameter trigger_change_fields | <p class="shorttext synchronized" lang="en">List of field that will trigger select on change...</p>
      "! E.g. MATNR when description should be updated from MAKT
      "! @parameter new_fields | <p class="shorttext synchronized" lang="en">All will be added to displayed grid...</p>
      "! New_name should be exactly the same as in query-select. Additional info in line type.
      "! @parameter query | <p class="shorttext synchronized" lang="en">Query info that will be run on field change...</p>
      "! Select should have same fields as in new_fields. Where condition should use '[at]row-[modified_data_ext field]'.
      "! Additional info in line type.
      constructor IMPORTING trigger_change_fields TYPE tt_trigger_change_fields new_fields TYPE tt_new_fields query TYPE t_query.

  PRIVATE SECTION.
    METHODS:
      run_query IMPORTING row TYPE any,
      update_row CHANGING row TYPE any.
    DATA:
      trigger_change_fields TYPE tt_trigger_change_fields,
      new_fields            TYPE tt_new_fields,
      query                 TYPE t_query,
      query_result          TYPE REF TO data.
ENDCLASS.

CLASS zcl_zabap_table_edit_descr_ext IMPLEMENTATION.
  METHOD constructor.
    me->trigger_change_fields = trigger_change_fields.
    me->new_fields = new_fields.
    me->query = query.

    "Will need struct to store query result
    DATA additional_fields  TYPE cl_abap_structdescr=>component_table.
    zif_zabap_table_edit_data~additional_fields( CHANGING additional_fields = additional_fields ).
    DATA(struct) = cl_abap_structdescr=>get( p_components = additional_fields ).
    CREATE DATA query_result TYPE HANDLE struct.
  ENDMETHOD.

  METHOD zif_zabap_table_edit_data~additional_fields.
    LOOP AT new_fields REFERENCE INTO DATA(new_field).
      APPEND VALUE #( name = new_field->new_name type = CAST #( cl_abap_typedescr=>describe_by_name( new_field->type_name ) ) ) TO additional_fields.
    ENDLOOP.
  ENDMETHOD.

  METHOD zif_zabap_table_edit_data~additional_validation.
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
  ENDMETHOD.

  METHOD zif_zabap_table_edit_data~on_data_changed.
    FIELD-SYMBOLS <modified_data> TYPE table.
    ASSIGN er_data_changed->mp_mod_rows->* TO <modified_data>.

    LOOP AT er_data_changed->mt_mod_cells REFERENCE INTO DATA(modified_cell).
      IF line_exists( trigger_change_fields[ table_line = modified_cell->fieldname ] ).
        ASSIGN <modified_data>[ modified_cell->tabix ] TO FIELD-SYMBOL(<row>).
        run_query( <row> ).

        ASSIGN query_result->* TO FIELD-SYMBOL(<query_result>).
        LOOP AT new_fields REFERENCE INTO DATA(new_field).
          ASSIGN COMPONENT new_field->new_name OF STRUCTURE <query_result> TO FIELD-SYMBOL(<new_value>).
          er_data_changed->modify_cell( i_row_id = modified_cell->row_id i_fieldname = new_field->new_name  i_value = <new_value> ).
        ENDLOOP.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.

  METHOD zif_zabap_table_edit_data~on_data_changed_finished.
  ENDMETHOD.

  METHOD zif_zabap_table_edit_data~refresh_grid.
    FIELD-SYMBOLS <modified_data_ext> TYPE table.
    ASSIGN modified_data_ext->* TO <modified_data_ext>.

    LOOP AT <modified_data_ext> ASSIGNING FIELD-SYMBOL(<row>).
      run_query( <row> ).
      update_row( CHANGING row = <row> ).
    ENDLOOP.

    LOOP AT new_fields REFERENCE INTO DATA(new_field).
      field_catalogue[ KEY name fieldname = new_field->new_name ]-edit = abap_false.
      IF NOT new_field->after_field IS INITIAL.
        field_catalogue[ KEY name fieldname = new_field->new_name ]-col_pos = field_catalogue[ KEY name fieldname = new_field->after_field ]-col_pos.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.

  METHOD zif_zabap_table_edit_commands~set_edit_mode.
  ENDMETHOD.

  METHOD zif_zabap_table_edit_data~change_display_text.
  ENDMETHOD.
  METHOD update_row.
    ASSIGN query_result->* TO FIELD-SYMBOL(<query_result>).
    LOOP AT new_fields REFERENCE INTO DATA(new_field).
      ASSIGN COMPONENT new_field->new_name OF STRUCTURE row TO FIELD-SYMBOL(<old_value>).
      ASSIGN COMPONENT new_field->new_name OF STRUCTURE <query_result> TO FIELD-SYMBOL(<new_value>).
      <old_value> = <new_value>.
    ENDLOOP.
  ENDMETHOD.

  METHOD run_query.
    ASSIGN query_result->* TO FIELD-SYMBOL(<query_result>).
    CLEAR <query_result>.
    SELECT SINGLE (query-select) FROM (query-from) WHERE (query-where) INTO CORRESPONDING FIELDS OF @<query_result>.
  ENDMETHOD.

  METHOD zif_zabap_table_edit_config~change_init_selection_fields.

  ENDMETHOD.

ENDCLASS.
