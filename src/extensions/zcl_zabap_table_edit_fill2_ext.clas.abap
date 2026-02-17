"! <p class="shorttext synchronized">Additional validation whether selected fields were filled</p>
"!
CLASS zcl_zabap_table_edit_fill2_ext DEFINITION PUBLIC CREATE PUBLIC INHERITING FROM zcl_zabap_table_edit_row_stat.

  PUBLIC SECTION.
    TYPES:
      tt_fields TYPE RANGE OF fieldname.

    METHODS:
      "! @parameter fields_restriction | <p class="shorttext synchronized" lang="en">Fields that must be non-initial. MANDT is always included...</p>
      "! Should be added manually if has another name
      constructor IMPORTING fields_restriction TYPE tt_fields OPTIONAL,
      zif_zabap_table_edit_data~additional_validation REDEFINITION,
      zif_zabap_table_edit_data~initial_data REDEFINITION.

  PRIVATE SECTION.
    DATA:
      components         TYPE cl_abap_structdescr=>component_table,
      fields_restriction TYPE tt_fields.
ENDCLASS.

CLASS zcl_zabap_table_edit_fill2_ext IMPLEMENTATION.
  METHOD constructor.
    super->constructor( ).
    me->fields_restriction = fields_restriction.
    APPEND VALUE #( sign = 'E' option = 'EQ' low = 'MANDT' ) TO me->fields_restriction.
  ENDMETHOD.

  METHOD zif_zabap_table_edit_data~additional_validation.
    super->zif_zabap_table_edit_data~additional_validation( CHANGING result = result all_modified_data = all_modified_data compared = compared ).

    FIELD-SYMBOLS <table> TYPE table.
    ASSIGN all_modified_data->* TO <table>.

    clear_all_rows( ).

    LOOP AT components REFERENCE INTO DATA(component).
      LOOP AT <table> ASSIGNING FIELD-SYMBOL(<row>).
        DATA(row_index) = sy-tabix.
        ASSIGN COMPONENT component->name OF STRUCTURE <row> TO FIELD-SYMBOL(<component_value>).
        IF <component_value> IS INITIAL.
          DATA(elemdescr) = CAST cl_abap_elemdescr( component->type ).
          DATA(ddic_field) = elemdescr->get_ddic_field( ).
          IF ddic_field-domname <> 'XFELD' AND ddic_field-domname <> 'XFIELD'. "Skip true/false checkboxes
            MESSAGE e018(zabap_table_edit) WITH ddic_field-reptext INTO DATA(msg).
            set_status( index = row_index status = c_status-error ).
            add_to_color( index = row_index fname = CONV #( component->name ) color = CORRESPONDING #( c_color-red ) ).
            add_to_msg( index = row_index msg = msg ).
            result = zif_zabap_table_edit_data~c_validation-extension_invalid.
          ENDIF.
        ENDIF.
      ENDLOOP.
    ENDLOOP.

    refresh_grid( ).

    IF result = zif_zabap_table_edit_data~c_validation-extension_invalid.
      MESSAGE s019(zabap_table_edit) DISPLAY LIKE 'E'.
    ENDIF.
  ENDMETHOD.

  METHOD zif_zabap_table_edit_data~initial_data.
    super->zif_zabap_table_edit_data~initial_data( CHANGING initial_data = initial_data ).

    DATA(table_descr) = CAST cl_abap_tabledescr( cl_abap_structdescr=>describe_by_data_ref( initial_data ) ).
    DATA(line_descr) = CAST cl_abap_structdescr( table_descr->get_table_line_type( ) ).
    components = line_descr->get_components( ).
    DELETE components WHERE NOT name IN fields_restriction.
  ENDMETHOD.
ENDCLASS.
