INTERFACE zif_zabap_table_edit_data PUBLIC.
  METHODS:
    "! <p class="shorttext synchronized">Can be used to disable execution of default select</p>
    "! @parameter execute | <p class="shorttext synchronized">If set to abap_false default select will not be executed</p>
    default_select CHANGING execute TYPE abap_bool,
    "! <p class="shorttext synchronized">Display / table edit will be based on this data.</p>
    "! @parameter initial_data | <p class="shorttext synchronized">Ref to table with all/none data from table...</p>
    "! depending on whether defaulet_select was executed or not.
    "! All insert/deletion/modify will be executed in according to changes compared to this table.
    initial_data CHANGING initial_data TYPE REF TO data,
    "! <p class="shorttext synchronized">Allows to extend displayed table - fill fields manually... </p>
    "! E.g. with MAKTX for material description - must be filled later on e.g. in change events
    "! @parameter additional_fields | <p class="shorttext synchronized">Names must be unique and different from 'CHANGE_INDICATOR'</p>
    additional_fields CHANGING additional_fields TYPE cl_abap_structdescr=>component_table,
    "! <p class="shorttext synchronized">Called whenever display mode is changed/refreshed, ...</p>
    "! before setting grid. Not called when user edits data.
    "! @parameter in_edit_mode | <p class="shorttext synchronized">If called in edit mode</p>
    "! @parameter field_catalogue | <p class="shorttext synchronized"></p>
    "! @parameter initial_data | <p class="shorttext synchronized">Ref to table of given type with non-modified data</p>
    "! @parameter modified_data_ext | <p class="shorttext synchronized"></p>
    refresh_grid IMPORTING in_edit_mode TYPE abap_bool
                 CHANGING field_catalogue TYPE zcl_zabap_field_catalogue=>tt_field_cat
                          initial_data TYPE REF TO data modified_data_ext TYPE REF TO data,
    "! <p class="shorttext synchronized">After internal validation. Tables are base, not extended</p>
    "! @parameter result | <p class="shorttext synchronized">Of type c_validation in this interface</p>
    "! @parameter all_modified_data | <p class="shorttext synchronized"></p>
    "! @parameter duplicates | <p class="shorttext synchronized"></p>
    "! @parameter inserted | <p class="shorttext synchronized"></p>
    "! @parameter deleted | <p class="shorttext synchronized"></p>
    "! @parameter before_modified | <p class="shorttext synchronized"></p>
    "! @parameter modified | <p class="shorttext synchronized"></p>
    additional_validation CHANGING result TYPE i all_modified_data TYPE REF TO data compared type zcl_zabap_table_edit_globals=>t_data_comparision,
    "! <p class="shorttext synchronized">After validation, before save. Base tables, not extended...</p>
    "! Can be used to further modify data or cancel with MESSAGE 'E'. Can be used to insert additional data to other tables,
    "! since COMMIT is called after this function and before <em>after_save</em> function
    "! @parameter inserted | <p class="shorttext synchronized"></p>
    "! @parameter deleted | <p class="shorttext synchronized"></p>
    "! @parameter before_modified | <p class="shorttext synchronized"></p>
    "! @parameter modified | <p class="shorttext synchronized"></p>
    before_save CHANGING compared type zcl_zabap_table_edit_globals=>t_data_comparision,
    "! <p class="shorttext synchronized">After save and COMMIT. Tables are base, not extended</p>
    "! @parameter inserted | <p class="shorttext synchronized"></p>
    "! @parameter deleted | <p class="shorttext synchronized"></p>
    "! @parameter before_modified | <p class="shorttext synchronized"></p>
    "! @parameter modified | <p class="shorttext synchronized"></p>
    after_save CHANGING compared type zcl_zabap_table_edit_globals=>t_data_comparision,
    "! <p class="shorttext synchronized" lang="en">Called before display, after grid refresh</p>
    change_display_text changing display_text TYPE string.

  METHODS:
    "! <p class="shorttext synchronized">CL_GUI_ALV_GRID event - hooked up automatically</p>
    on_data_changed FOR EVENT data_changed OF cl_gui_alv_grid IMPORTING er_data_changed e_onf4 e_onf4_before e_onf4_after e_ucomm sender,
    "! <p class="shorttext synchronized">CL_GUI_ALV_GRID event - hooked up automatically</p>
    on_data_changed_finished FOR EVENT data_changed_finished OF cl_gui_alv_grid IMPORTING e_modified et_good_cells sender.
ENDINTERFACE.
