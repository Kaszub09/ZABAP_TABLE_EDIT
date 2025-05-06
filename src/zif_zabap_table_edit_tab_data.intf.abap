INTERFACE zif_zabap_table_edit_tab_data PUBLIC.
  TYPES:
    BEGIN OF t_config,
      table_name           TYPE string,
      change_doc_type      TYPE zabap_change_doc_type,
      disable_text_table   TYPE abap_bool,
      disable_selection    TYPE abap_bool,
      show_selection_first TYPE abap_bool,
      BEGIN OF ext,
        commands TYPE REF TO zif_zabap_table_edit_commands,
        config   TYPE REF TO zif_zabap_table_edit_config,
        data     TYPE REF TO zif_zabap_table_edit_data,
      END OF ext,
    END OF t_config.

  METHODS:
    lock_table EXPORTING error_message TYPE string RETURNING VALUE(locked) TYPE abap_bool,
    unlock_table,
    "! <p class="shorttext synchronized">Recreate table and field catalogue for grid</p>
    "! @parameter in_edit_mode | <p class="shorttext synchronized"></p>
    reset_grid IMPORTING in_edit_mode TYPE abap_bool,
    "! <p class="shorttext synchronized">Compare table and check fields with checktables</p>
    "! @parameter result |<p class="shorttext synchronized">Of type <em>zcl_zabap_table_edit_globals-c_validation</em></p>
    validate EXPORTING result TYPE i compared TYPE zcl_zabap_table_edit_globals=>t_data_comparision,
    save_data EXPORTING erorr_message TYPE string CHANGING compared TYPE zcl_zabap_table_edit_globals=>t_data_comparision
              RETURNING VALUE(sucess) TYPE abap_bool,
    get_selected_row_key RETURNING VALUE(tabkey) TYPE string,
    restrict_selection RETURNING VALUE(changed) TYPE abap_bool RAISING zcx_zabap_table_edit,
    switch_tech_display.


  DATA:
    mandant_field    TYPE string READ-ONLY,
    was_data_changed TYPE abap_bool READ-ONLY.
ENDINTERFACE.
