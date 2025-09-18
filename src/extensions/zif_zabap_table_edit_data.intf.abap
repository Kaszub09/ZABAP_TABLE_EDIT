INTERFACE zif_zabap_table_edit_data PUBLIC.
  TYPES:
    BEGIN OF t_data_comparision,
      duplicates       TYPE REF TO data,
      inserted         TYPE REF TO data,
      deleted          TYPE REF TO data,
      before_modified  TYPE REF TO data,
      modified         TYPE REF TO data,
      not_in_selection TYPE REF TO data,
    END OF t_data_comparision.

  CONSTANTS:
    BEGIN OF c_validation,
      incorrect_values  TYPE i VALUE 0,
      duplicates        TYPE i VALUE 1,
      ok                TYPE i VALUE 2,
      extension_invalid TYPE i VALUE 3,
      not_in_selection  TYPE i VALUE 4,
    END OF c_validation.

  METHODS:
    "! <p class="shorttext synchronized">Can be used to disable execution of default select</p>
    "! @parameter execute | <p class="shorttext synchronized">If set to abap_false default select will not be executed</p>
    default_select DEFAULT IGNORE CHANGING execute TYPE abap_bool,
    "! <p class="shorttext synchronized">Display / table edit will be based on this data.</p>
    "! @parameter initial_data | <p class="shorttext synchronized">Ref to table with all/none data from table...</p>
    "! depending on whether defaulet_select was executed or not.
    "! All insert/deletion/modify will be executed in according to changes compared to this table.
    initial_data DEFAULT IGNORE CHANGING initial_data TYPE REF TO data,
    "! <p class="shorttext synchronized">Allows to extend displayed table - fill fields manually... </p>
    "! E.g. with MAKTX for material description - must be filled later on e.g. in change events
    "! @parameter additional_fields | <p class="shorttext synchronized">Names must be unique compared to table</p>
    additional_fields DEFAULT IGNORE CHANGING additional_fields TYPE cl_abap_structdescr=>component_table,
    "! <p class="shorttext synchronized">Called whenever display mode is changed/refreshed, ...</p>
    "! before setting grid. Not called when user edits data.
    "! @parameter in_edit_mode | <p class="shorttext synchronized">If called in edit mode</p>
    "! @parameter initial_data | <p class="shorttext synchronized">Ref to table of given type with non-modified data</p>
    refresh_grid DEFAULT IGNORE IMPORTING in_edit_mode TYPE abap_bool
                 CHANGING field_catalogue TYPE zcl_zabap_field_catalogue=>tt_field_cat
                          layout TYPE lvc_s_layo variant TYPE disvariant
                          initial_data TYPE REF TO data modified_data_ext TYPE REF TO data,
    "! <p class="shorttext synchronized">After internal validation. Tables are base, not extended</p>
    "! @parameter result | <p class="shorttext synchronized">Of type zcl_zabap_table_edit_globals-c_validation</p>
    additional_validation DEFAULT IGNORE CHANGING result TYPE i all_modified_data TYPE REF TO data compared TYPE t_data_comparision,
    "! <p class="shorttext synchronized">After validation, before save. Base tables, not extended...</p>
    "! Can be used to further modify data or cancel with MESSAGE 'E'. Can be used to insert additional data to other tables,
    "! since COMMIT is called after this function and before <em>after_save</em> function
    before_save DEFAULT IGNORE CHANGING compared TYPE t_data_comparision,
    "! <p class="shorttext synchronized">After save and COMMIT. Tables are base, not extended</p>
    after_save DEFAULT IGNORE CHANGING compared TYPE t_data_comparision,
    "! <p class="shorttext synchronized" lang="en">Called before display, after <em>refresh_grid</em></p>
    change_display_text DEFAULT IGNORE CHANGING display_text TYPE string.

  METHODS:
    "! <p class="shorttext synchronized">CL_GUI_ALV_GRID event - hooked up to grid automatically</p>
    on_data_changed DEFAULT IGNORE FOR EVENT data_changed OF zif_zabap_table_edit_grid_if IMPORTING er_data_changed e_onf4 e_onf4_before e_onf4_after e_ucomm sender,
    "! <p class="shorttext synchronized">CL_GUI_ALV_GRID event - hooked up to grid automatically</p>
    on_data_changed_finished DEFAULT IGNORE FOR EVENT data_changed_finished OF zif_zabap_table_edit_grid_if IMPORTING e_modified et_good_cells sender.
ENDINTERFACE.
