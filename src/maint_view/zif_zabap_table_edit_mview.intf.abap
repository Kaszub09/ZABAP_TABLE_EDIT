INTERFACE zif_zabap_table_edit_mview PUBLIC.
  METHODS:
    map_table_ext_view_to_base CHANGING extended_view TYPE table base TYPE table,
    map_table_base_to_ext_view CHANGING base TYPE table extended_view TYPE table,
    map_struct_ext_view_to_base CHANGING extended_view TYPE any base TYPE any,
    run_initial_select CHANGING initial_data TYPE table,
    get_ext_view                   IMPORTING additional_fields TYPE abap_component_tab RETURNING VALUE(ext_view) TYPE REF TO data,
    get_ext_view_fc_with_add_field IMPORTING in_edit_mode TYPE abap_bool additional_fields TYPE abap_component_tab
                                   RETURNING VALUE(fc) TYPE zcl_zabap_field_catalogue=>tt_field_cat,
    update_non_base_fields CHANGING extended_view TYPE table,
    update_view_values IMPORTING er_data_changed TYPE REF TO cl_alv_changed_data_protocol.

  DATA:
    base_table   TYPE string READ-ONLY,
    same_as_base TYPE abap_bool READ-ONLY.

ENDINTERFACE.
