CLASS zcl_zabap_table_edit_mview_e DEFINITION PUBLIC CREATE PUBLIC.

  PUBLIC SECTION.
    INTERFACES:
      zif_zabap_table_edit_mview.

    ALIASES:
     base_table FOR zif_zabap_table_edit_mview~base_table,
     same_as_base FOR zif_zabap_table_edit_mview~same_as_base.

    METHODS:
      constructor IMPORTING view_name TYPE string.

    DATA:
       fields TYPE REF TO zcl_zabap_table_fields.
ENDCLASS.

CLASS zcl_zabap_table_edit_mview_e IMPLEMENTATION.
  METHOD constructor.
    base_table = view_name.
    same_as_base = abap_true.
    fields = NEW zcl_zabap_table_fields( base_table ).
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
    base = CORRESPONDING #( extended_view ).
  ENDMETHOD.

  METHOD zif_zabap_table_edit_mview~map_table_base_to_ext_view.
    extended_view = CORRESPONDING #( base ).
  ENDMETHOD.

  METHOD zif_zabap_table_edit_mview~map_table_ext_view_to_base.
    base = CORRESPONDING #( extended_view ).
  ENDMETHOD.

  METHOD zif_zabap_table_edit_mview~run_initial_select.
    SELECT * FROM (base_table) INTO TABLE @initial_data ORDER BY PRIMARY KEY.
  ENDMETHOD.

  METHOD zif_zabap_table_edit_mview~update_non_base_fields.
  ENDMETHOD.

  METHOD zif_zabap_table_edit_mview~update_view_values.
  ENDMETHOD.
ENDCLASS.
