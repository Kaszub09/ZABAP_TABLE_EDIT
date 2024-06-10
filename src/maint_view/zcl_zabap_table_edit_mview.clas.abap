CLASS zcl_zabap_table_edit_mview DEFINITION PUBLIC CREATE PUBLIC.

  PUBLIC SECTION.
    TYPES:
      BEGIN OF t_mappings,
        table_view_to_base  TYPE cl_abap_corresponding=>mapping_table,
        table_base_to_view  TYPE cl_abap_corresponding=>mapping_table,
        struct_view_to_base TYPE cl_abap_corresponding=>mapping_table,
      END OF t_mappings.

    INTERFACES:
      zif_zabap_table_edit_mview.

    ALIASES:
     base_table FOR zif_zabap_table_edit_mview~base_table,
     same_as_base FOR zif_zabap_table_edit_mview~same_as_base.

    METHODS:
      constructor IMPORTING view_name TYPE string.

    DATA:
      view_name TYPE string,
      fields    TYPE REF TO zcl_zabap_table_fields,
      mappings  TYPE t_mappings,
      tables    TYPE STANDARD TABLE OF dd27d.
ENDCLASS.

CLASS zcl_zabap_table_edit_mview IMPLEMENTATION.
  METHOD constructor.
    me->view_name = view_name.

    SELECT SINGLE roottab FROM dd25l WHERE viewname = @view_name INTO @base_table.
    same_as_base = abap_false.
    fields = NEW zcl_zabap_table_fields( view_name ).

    "Get fields mapping
    SELECT FROM dd27s
      FIELDS viewfield AS view_field, tabname AS base_tab, fieldname AS base_field
      WHERE viewname = @view_name
      ORDER BY objpos ASCENDING
      INTO TABLE @DATA(fields_mapping).
    mappings-table_view_to_base  = VALUE #( FOR fm IN fields_mapping WHERE ( base_tab = base_table ) ( kind = 1 srcname = fm-view_field dstname = fm-base_field ) ).
    mappings-struct_view_to_base = VALUE #( FOR fm IN fields_mapping WHERE ( base_tab = base_table ) ( kind = 1 srcname = fm-view_field dstname = fm-base_field ) ).
    mappings-table_base_to_view  = VALUE #( FOR fm IN fields_mapping WHERE ( base_tab = base_table ) ( kind = 1 srcname = fm-base_field dstname = fm-view_field ) ).
  ENDMETHOD.

  METHOD zif_zabap_table_edit_mview~get_ext_view.
    fields->get_base_with_add_fields( EXPORTING additional_fields = additional_fields IMPORTING table = DATA(table) ).
    CREATE DATA ext_view TYPE HANDLE table.
  ENDMETHOD.

  METHOD zif_zabap_table_edit_mview~get_ext_view_fc_with_add_field.
    fields->set_edit_mode( in_edit_mode ).
    fc = fields->get_fc_with_add_fields( additional_fields ).
  ENDMETHOD.

  METHOD zif_zabap_table_edit_mview~map_struct_ext_view_to_base.
    cl_abap_corresponding=>create( source = extended_view destination = base mapping = mappings-table_view_to_base
       )->execute( EXPORTING source = extended_view CHANGING destination = base ).
  ENDMETHOD.

  METHOD zif_zabap_table_edit_mview~map_table_base_to_ext_view.
    cl_abap_corresponding=>create( source = base destination = extended_view mapping = mappings-table_base_to_view
       )->execute( EXPORTING source = base CHANGING destination = extended_view ).
  ENDMETHOD.

  METHOD zif_zabap_table_edit_mview~map_table_ext_view_to_base.
    cl_abap_corresponding=>create( source = extended_view destination = base mapping = mappings-struct_view_to_base
       )->execute( EXPORTING source = extended_view CHANGING destination = base ).
  ENDMETHOD.

  METHOD zif_zabap_table_edit_mview~run_initial_select.
    "TODO - Restrict based on view
    SELECT * FROM (base_table) INTO TABLE @initial_data ORDER BY PRIMARY KEY.
  ENDMETHOD.

  METHOD zif_zabap_table_edit_mview~update_non_base_fields.
  "An attempt was made! But kinda too much complicated to dynamically buid query using foreign keys from view and map it to view fields
    SELECT FROM dd26s
                INNER JOIN dd05p ON dd05p~tabname = dd26s~fortabname AND dd05p~fieldname = dd26s~forfield AND dd26s~fordir = 'E'
                LEFT JOIN dd27s AS dd27s_source ON dd27s_source~viewname = dd26s~viewname AND dd27s_source~tabname = dd05p~fortable
                                AND dd27s_source~fieldname = dd05p~forkey
                LEFT JOIN dd27s AS dd27s_join ON dd27s_join~viewname = dd26s~viewname AND dd27s_join~tabname = dd05p~checktable
                                AND dd27s_join~fieldname = dd05p~checkfield
      FIELDS dd26s~fortabname AS source_tab, dd26s~forfield AS source_tab_key_field, dd26s~tabname AS join_tab,
             dd05p~forkey AS source_tab_field, dd05p~checkfield AS join_tab_field,
             dd27s_source~viewfield AS source_tab_view_field, dd27s_join~viewfield AS join_tab_view_field
      WHERE dd26s~viewname = @view_name
    UNION
    SELECT FROM dd26s
                INNER JOIN dd05p ON dd05p~tabname = dd26s~tabname AND dd05p~fieldname = dd26s~forfield AND dd26s~fordir = 'I'
                LEFT JOIN dd27s AS dd27s_source ON dd27s_source~viewname = dd26s~viewname AND dd27s_source~tabname = dd05p~checktable
                                AND dd27s_source~fieldname = dd05p~checkfield
                LEFT JOIN dd27s AS dd27s_join ON dd27s_join~viewname = dd26s~viewname AND dd27s_join~tabname = dd05p~fortable
                                AND dd27s_join~fieldname = dd05p~forkey
      FIELDS dd26s~fortabname AS source_tab, dd26s~forfield AS source_tab_key_field, dd26s~tabname AS join_tab,
             dd05p~checkfield AS source_tab_field, dd05p~forkey AS join_tab_field,
             dd27s_source~viewfield AS source_tab_view_field, dd27s_join~viewfield AS join_tab_view_field
      WHERE dd26s~viewname = @view_name
    INTO TABLE @DATA(joins_and_mappings).


    "TODO
    LOOP AT extended_view ASSIGNING FIELD-SYMBOL(<row>).

    ENDLOOP.
  ENDMETHOD.

  METHOD zif_zabap_table_edit_mview~update_view_values.
    "TODO
  ENDMETHOD.
ENDCLASS.
