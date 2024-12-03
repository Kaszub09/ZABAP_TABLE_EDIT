INTERFACE zif_zabap_table_edit_grid_if PUBLIC.
  DATA:
      "! <p class="shorttext synchronized" >Can be used by extension, since...</p>
      "! interface implements only methods that are required internally
       grid TYPE REF TO cl_gui_alv_grid READ-ONLY.
  "--------------------------------------------------
  METHODS
    register_edit_event
      IMPORTING
        VALUE(i_event_id) TYPE i
      EXCEPTIONS
        error.
  "--------------------------------------------------
  METHODS get_selected_rows
    EXPORTING
      !et_index_rows TYPE lvc_t_row
      !et_row_no     TYPE lvc_t_roid .
  "--------------------------------------------------
  METHODS get_selected_cells
    EXPORTING
      !et_cell TYPE lvc_t_cell .
  "--------------------------------------------------
  METHODS set_table_for_first_display
    IMPORTING
      VALUE(i_buffer_active)      TYPE any OPTIONAL
      VALUE(i_bypassing_buffer)   TYPE char01 OPTIONAL
      VALUE(i_consistency_check)  TYPE char1 OPTIONAL
      VALUE(i_structure_name)     TYPE dd02l-tabname OPTIONAL
      VALUE(is_variant)           TYPE disvariant OPTIONAL
      VALUE(i_save)               TYPE char01 OPTIONAL
      VALUE(i_default)            TYPE char01 DEFAULT 'X'
      VALUE(is_layout)            TYPE lvc_s_layo OPTIONAL
      VALUE(is_print)             TYPE lvc_s_prnt OPTIONAL
      VALUE(it_special_groups)    TYPE lvc_t_sgrp OPTIONAL
      VALUE(it_toolbar_excluding) TYPE ui_functions OPTIONAL
      VALUE(it_hyperlink)         TYPE lvc_t_hype OPTIONAL
      VALUE(it_alv_graphics)      TYPE dtc_t_tc OPTIONAL
      VALUE(it_except_qinfo)      TYPE lvc_t_qinf OPTIONAL
      !ir_salv_adapter            TYPE REF TO if_salv_adapter OPTIONAL
    CHANGING
      !it_outtab                  TYPE STANDARD TABLE
      VALUE(it_fieldcatalog)      TYPE lvc_t_fcat OPTIONAL
      VALUE(it_sort)              TYPE lvc_t_sort OPTIONAL
      VALUE(it_filter)            TYPE lvc_t_filt OPTIONAL.
  "--------------------------------------------------
  METHODS check_changed_data
    EXPORTING
      !e_valid   TYPE char01
    CHANGING
      !c_refresh TYPE char01 DEFAULT 'X' .
  "--------------------------------------------------
  METHODS refresh_table_display
    IMPORTING
      VALUE(is_stable)      TYPE lvc_s_stbl OPTIONAL
      VALUE(i_soft_refresh) TYPE char01 OPTIONAL.
  "--------------------------------------------------
  METHODS set_ready_for_input
    IMPORTING
      VALUE(i_ready_for_input) TYPE int4 DEFAULT 1.
  "=================================================================
  "-----------------------------------------------------------------
  EVENTS data_changed
   EXPORTING
     VALUE(er_data_changed) TYPE REF TO cl_alv_changed_data_protocol OPTIONAL
     VALUE(e_onf4) TYPE char01 OPTIONAL
     VALUE(e_onf4_before) TYPE char01 OPTIONAL
     VALUE(e_onf4_after) TYPE char01 OPTIONAL
     VALUE(e_ucomm) TYPE sy-ucomm OPTIONAL.
  "--------------------------------------------------
  EVENTS data_changed_finished
    EXPORTING
      VALUE(e_modified) TYPE char01 OPTIONAL
      VALUE(et_good_cells) TYPE lvc_t_modi OPTIONAL.
ENDINTERFACE.
