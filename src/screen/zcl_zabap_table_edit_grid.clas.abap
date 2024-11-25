CLASS zcl_zabap_table_edit_grid DEFINITION PUBLIC INHERITING FROM cl_gui_alv_grid CREATE PRIVATE
  GLOBAL FRIENDS zcl_zabap_table_edit_factory.

  PUBLIC SECTION.
    INTERFACES:
      zif_zabap_table_edit_grid_if.

    METHODS:
      constructor
        IMPORTING
          i_shellstyle  TYPE i DEFAULT 0
          i_lifetime    TYPE i OPTIONAL
          i_parent      TYPE REF TO cl_gui_container
          i_appl_events TYPE char01 DEFAULT space
          i_parentdbg          TYPE REF TO cl_gui_container OPTIONAL
          i_applogparent       TYPE REF TO cl_gui_container OPTIONAL
          i_graphicsparent     TYPE REF TO cl_gui_container OPTIONAL
          i_name        TYPE string OPTIONAL
          i_fcat_complete      TYPE sap_bool DEFAULT space.

  PRIVATE SECTION.
    METHODS:
      on_data_changed FOR EVENT data_changed OF cl_gui_alv_grid IMPORTING er_data_changed e_onf4 e_onf4_before e_onf4_after e_ucomm sender,
      on_data_changed_finished FOR EVENT data_changed_finished OF cl_gui_alv_grid IMPORTING e_modified et_good_cells sender.
ENDCLASS.

CLASS zcl_zabap_table_edit_grid IMPLEMENTATION.
  METHOD zif_zabap_table_edit_grid_if~check_changed_data.
    check_changed_data(
      IMPORTING
        e_valid   = e_valid                 " Entries are Consistent
      CHANGING
        c_refresh = c_refresh ). " Character Field of Length 1
  ENDMETHOD.

  METHOD zif_zabap_table_edit_grid_if~get_selected_cells.
    get_selected_cells(
      IMPORTING
        et_cell = et_cell ). " Selected Cells
  ENDMETHOD.

  METHOD zif_zabap_table_edit_grid_if~get_selected_rows.
    get_selected_rows(
      IMPORTING
        et_index_rows = et_index_rows                 " Indexes of Selected Rows
        et_row_no     = et_row_no ). " Numeric IDs of Selected Rows
  ENDMETHOD.

  METHOD zif_zabap_table_edit_grid_if~register_edit_event.
    register_edit_event(
        i_event_id = i_event_id  ).
  ENDMETHOD.

  METHOD zif_zabap_table_edit_grid_if~set_table_for_first_display.
    set_table_for_first_display(
      EXPORTING
        i_buffer_active               = i_buffer_active                " Buffering Active
        i_bypassing_buffer            = i_bypassing_buffer                 " Switch Off Buffer
        i_consistency_check           = i_consistency_check                 " Starting Consistency Check for Interface Error Recognition
        i_structure_name              = i_structure_name                 " Internal Output Table Structure Name
        is_variant                    = is_variant                 " Layout
        i_save                        = i_save                 " Save Layout
        i_default                     = i_default              " Default Display Variant
        is_layout                     = is_layout                 " Layout
        is_print                      = is_print                 " Print Control
        it_special_groups             = it_special_groups                 " Field Groups
        it_toolbar_excluding          = it_toolbar_excluding                 " Excluded Toolbar Standard Functions
        it_hyperlink                  = it_hyperlink                 " Hyperlinks
        it_alv_graphics               = it_alv_graphics                 " Table of Structure DTC_S_TC
        it_except_qinfo               = it_except_qinfo                 " Table for Exception Quickinfo
        ir_salv_adapter               = ir_salv_adapter                 " Interface ALV Adapter
      CHANGING
        it_outtab                     = it_outtab                 " Output Table
        it_fieldcatalog               = it_fieldcatalog                 " Field Catalog
        it_sort                       = it_sort                 " Sort Criteria
        it_filter                     = it_filter ). " Filter Criteria
  ENDMETHOD.

  METHOD constructor.
    super->constructor( i_shellstyle = i_shellstyle i_lifetime = i_lifetime i_parent = i_parent i_appl_events = i_appl_events
        i_parentdbg = i_parentdbg i_applogparent = i_applogparent i_graphicsparent = i_graphicsparent i_name = i_name
        i_fcat_complete = i_fcat_complete ).

    SET HANDLER on_data_changed FOR me.
    SET HANDLER on_data_changed_finished FOR me.

    zif_zabap_table_edit_grid_if~grid = me.
  ENDMETHOD.

  METHOD on_data_changed.
    RAISE EVENT zif_zabap_table_edit_grid_if~data_changed EXPORTING er_data_changed = er_data_changed
        e_onf4 = e_onf4 e_onf4_before = e_onf4_before e_ucomm = e_ucomm.
  ENDMETHOD.

  METHOD on_data_changed_finished.
    RAISE EVENT zif_zabap_table_edit_grid_if~data_changed_finished EXPORTING e_modified = e_modified et_good_cells = et_good_cells.
  ENDMETHOD.

  METHOD zif_zabap_table_edit_grid_if~refresh_table_display.
    refresh_table_display(
        is_stable      = is_stable                 " With Stable Rows/Columns
        i_soft_refresh = i_soft_refresh ). " Without Sort, Filter, etc.
  ENDMETHOD.
ENDCLASS.
