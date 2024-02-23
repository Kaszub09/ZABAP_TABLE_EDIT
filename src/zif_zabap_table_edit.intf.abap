interface ZIF_ZABAP_TABLE_EDIT
  PUBLIC
  .
  CONSTANTS:
    BEGIN OF c_validation,
      incorrect_values TYPE i VALUE 0,
      duplicates       TYPE i VALUE 1,
      ok               TYPE i VALUE 2,
    END OF c_validation.

  METHODS:
      "! <p class="shorttext synchronized">Display / table edit will be based on this data.</p>
      "! @parameter initial_data | <p class="shorttext synchronized">Ref to table of given type with all data from table...</p>
      "! All insert/deletion/modify will be executed in according to changes compared to this table
      initial_data CHANGING initial_data TYPE REF TO data,
      "! <p class="shorttext synchronized">Allows to extend displayed table - fill fields manually... </p>
      "! E.g. with MAKTX for material description - must be filled later on e.g. in change events
      "! @parameter additional_fields | <p class="shorttext synchronized">Names must be unique and different from 'CHANGE_INDICATOR'</p>
      additional_fields CHANGING additional_fields TYPE cl_abap_structdescr=>component_table,
      "! <p class="shorttext synchronized">Called whenever display mode is changed/refreshed, ...</p>
      "! before setting grid. Not called when user edits data.
      "! @parameter in_edit_mode | <p class="shorttext synchronized">If called in edit mode</p>
      "! @parameter initial_data | <p class="shorttext synchronized">Ref to table of given type with non-modified data</p>
      "! @parameter modified_data | <p class="shorttext synchronized">Table with add fields that will be displayed</p>
      refresh_grid IMPORTING in_edit_mode TYPE abap_bool
                   CHANGING field_catalogue TYPE zcl_zabap_field_catalogue=>tt_field_cat header_text type string
                            initial_data TYPE REF TO data modified_data_ext TYPE REF TO data,
      "! <p class="shorttext synchronized">Called whenever edit mode is changed</p>
      set_edit_mode CHANGING editable TYPE abap_bool,
      "! <p class="shorttext synchronized">Called once, e.g. modify commands / hook up other events</p>
      grid_setup CHANGING grid TYPE REF TO cl_gui_alv_grid,
      "! <p class="shorttext synchronized">Can execute custom command and alter default behavior</p>
      "! @parameter command | <p class="shorttext synchronized">Can be modified to alter called command</p>
      "! @parameter cancel_command | <p class="shorttext synchronized">Doesn't execute command</p>
      before_command CHANGING command TYPE syst_ucomm cancel_command TYPE abap_bool,
      "! <p class="shorttext synchronized">Not called if command was cancelled</p>
      after_command CHANGING command TYPE syst_ucomm,
      "! <p class="shorttext synchronized">Called after setting up the screen - edit class...</p>
      "! zcl_screen_with_container to modify/add commands to toolbar
      change_commands IMPORTING in_edit_mode TYPE abap_bool,
      "! <p class="shorttext synchronized">After internal validation. Tables are base, not extended</p>
      "! @parameter result | <p class="shorttext synchronized">Of type c_validation in this interface</p>
      additional_validation CHANGING result TYPE i all_modified_data TYPE REF TO data
                            duplicates TYPE REF TO data inserted TYPE REF TO data deleted TYPE REF TO data
                            before_modified TYPE REF TO data modified TYPE REF TO data,
       "! <p class="shorttext synchronized">After validation, before save. Base tables, not extended...</p>
       "! Can be used to further modify data or cancel with MESSAGE 'E'
      before_save CHANGING inserted TYPE REF TO data deleted TYPE REF TO data before_modified TYPE REF TO data modified TYPE REF TO data,
      "! <p class="shorttext synchronized">After save. Tables are base, not extended</p>
      after_save CHANGING inserted TYPE REF TO data deleted TYPE REF TO data before_modified TYPE REF TO data modified TYPE REF TO data.

  METHODS:
    "! <p class="shorttext synchronized">CL_GUI_ALV_GRID event - hooked up automatically</p>
    on_data_changed FOR EVENT data_changed OF cl_gui_alv_grid IMPORTING er_data_changed e_onf4 e_onf4_before e_onf4_after e_ucomm sender ,
    "! <p class="shorttext synchronized">CL_GUI_ALV_GRID event - hooked up automatically</p>
    on_data_changed_finished FOR EVENT data_changed_finished OF cl_gui_alv_grid IMPORTING e_modified et_good_cells sender.
ENDINTERFACE.
