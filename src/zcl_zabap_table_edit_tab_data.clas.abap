CLASS zcl_zabap_table_edit_tab_data DEFINITION PUBLIC FINAL CREATE PRIVATE GLOBAL FRIENDS zcl_zabap_table_edit_factory.

  PUBLIC SECTION.
    INTERFACES zif_zabap_table_edit_tab_data.

    ALIASES:
     mandant_field FOR zif_zabap_table_edit_tab_data~mandant_field,
     was_data_changed FOR zif_zabap_table_edit_tab_data~was_data_changed.

    METHODS:
      constructor IMPORTING configuration TYPE zif_zabap_table_edit_tab_data=>t_config grid TYPE REF TO zif_zabap_table_edit_grid_if.

  PRIVATE SECTION.
    TYPES:
      BEGIN OF t_table,
        initial_data      TYPE REF TO data,
        additional_fields TYPE cl_abap_structdescr=>component_table,
        "! <p class="shorttext synchronized">Original tab + add. fields. Displayed on screen in grid.</p>
        modified_data_ext TYPE REF TO data,
        fields            TYPE REF TO zcl_zabap_table_fields,
        locker            TYPE REF TO zcl_zabap_table_edit_lock,
        comparator        TYPE REF TO zcl_zabap_table_comparator,
        text_table        TYPE REF TO zif_zabap_table_edit_text_tab,
        db                TYPE REF TO zif_zabap_table_edit_db,
        selection         TYPE REF TO zif_zabap_table_edit_restr_sel,
      END OF t_table.

    METHODS:
      setup_grid,
      "! <p class="shorttext synchronized">Initial query from specified table</p>
      prepare_initial_data,
      create_change_doc IMPORTING compared TYPE zcl_zabap_table_edit_globals=>t_data_comparision  RAISING zcx_zabap_table_edit,
      "! <p class="shorttext synchronized">Display table to original tab - needed if fields were added</p>
      get_modified_data_no_ext RETURNING VALUE(modified_data) TYPE REF TO data,
      get_selected_row_index RETURNING VALUE(index) TYPE i,
      remove_empty_rows,
      get_not_in_selection IMPORTING compared                TYPE zcl_zabap_table_edit_globals=>t_data_comparision
                           RETURNING VALUE(not_in_selection) TYPE REF TO data.

    METHODS:
      on_data_changed FOR EVENT data_changed OF zif_zabap_table_edit_grid_if IMPORTING er_data_changed e_onf4 e_onf4_before e_onf4_after e_ucomm sender.

    DATA:
      table TYPE t_table,
      grid  TYPE REF TO zif_zabap_table_edit_grid_if.

    DATA:
      config          TYPE zif_zabap_table_edit_tab_data=>t_config.
ENDCLASS.



CLASS ZCL_ZABAP_TABLE_EDIT_TAB_DATA IMPLEMENTATION.


  METHOD constructor.
    config = configuration.

    table-fields     = NEW #( config-table_name ). "TODO as interface
    table-locker     = NEW #( config-table_name ). "TODO as interface
    table-comparator = NEW #( config-table_name ). "TODO as interface

    "---TEXT TABLE---
    table-text_table = zcl_zabap_table_edit_factory=>get_text_table( CORRESPONDING #( me->config ) ).
    table-text_table->append_additional_fields( CHANGING additional_fields = table-additional_fields ).

    me->grid = grid.
    setup_grid( ).

    "---EXTENSION CALL---
    config-ext-data->additional_fields( CHANGING additional_fields = table-additional_fields ).

    "---SELECTION---
    table-selection = zcl_zabap_table_edit_factory=>get_restrict_selection( CORRESPONDING #( config ) ).
    IF config-disable_selection = abap_false AND  config-show_selection_first = abap_true.
      table-selection->display( abap_false ).
    ENDIF.
    prepare_initial_data( ).
    table-db = zcl_zabap_table_edit_factory=>get_db( ).
  ENDMETHOD.


  METHOD create_change_doc.
    DATA(cd) = zcl_zabap_table_edit_factory=>get_change_doc( objectclass = CONV #( config-table_name ) objectid = CONV #( config-table_name ) ).

    cd->open( ).
    cd->change_multi( force_cd_on_all_fields = xsdbool( config-change_doc_type = 'F' )
                       table_name = config-table_name
                       deleted = cd->create_table_with_indicator( table_name = config-table_name original_table = compared-deleted indicator = 'D' )
                       inserted = cd->create_table_with_indicator( table_name = config-table_name original_table = compared-inserted  indicator = 'I' )
                       before_modified = cd->create_table_with_indicator( table_name = config-table_name original_table = compared-before_modified  indicator = 'U' )
                       modified = cd->create_table_with_indicator( table_name = config-table_name original_table = compared-modified  ) ).

    "Some SAP magic to get initial t-code
    DATA: original_tcode TYPE sytcode.
    CALL 'GET_PARAM_TCOD' ID 'PTCOD' FIELD original_tcode.
    cd->close( tcode = original_tcode ).
  ENDMETHOD.


  METHOD get_modified_data_no_ext.
    CREATE DATA modified_data TYPE TABLE OF (config-table_name).

    assign_to_table_fs modified_data->* <modified_data>.
    assign_to_table_fs table-modified_data_ext->* <modified_data_ext>.

    <modified_data> = CORRESPONDING #( <modified_data_ext> ).
  ENDMETHOD.


  METHOD get_not_in_selection.
    assign_to_table_fs compared-inserted->* <inserted>.
    assign_to_table_fs compared-modified->* <modified>.

    CREATE DATA not_in_selection LIKE <inserted>.
    assign_to_table_fs not_in_selection->* <not_in_selection>.

    APPEND LINES OF <inserted> TO <not_in_selection>.
    APPEND LINES OF <modified> TO <not_in_selection>.

    DATA(field_ranges) = table-selection->get_field_ranges( ).

    "Do it manually because undermentioned doesn't work (problem with table indexes?).
    ""DELETE <not_in_selection> WHERE ('field_ranges[ 1 ]-fieldname IN field_ranges[ 1 ]-selopt_t')"
    LOOP AT <not_in_selection> ASSIGNING FIELD-SYMBOL(<row>).
      DATA(idx) = sy-tabix.
      DATA(row_is_valid) = abap_true.
      LOOP AT field_ranges REFERENCE INTO DATA(field_range).
        ASSIGN COMPONENT field_range->fieldname OF STRUCTURE <row> TO FIELD-SYMBOL(<field>).
        IF NOT <field> IN field_range->selopt_t.
          row_is_valid = abap_false.
          EXIT.
        ENDIF.
      ENDLOOP.
      IF row_is_valid = abap_true.
        DELETE <not_in_selection> INDEX idx.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.


  METHOD get_selected_row_index.
    grid->get_selected_rows( IMPORTING et_index_rows = DATA(selected_rows) ).
    IF lines( selected_rows ) = 1.
      index = selected_rows[ 1 ]-index.
    ENDIF.
  ENDMETHOD.


  METHOD on_data_changed.
    was_data_changed = abap_true.
  ENDMETHOD.


  METHOD prepare_initial_data.
    "TODO   maint view?
    CREATE DATA table-initial_data TYPE TABLE OF (config-table_name).
    assign_to_table_fs table-initial_data->* <initial_data>.

    table-fields->get_base_with_add_fields( EXPORTING additional_fields = table-additional_fields IMPORTING table = DATA(table_descr) ).
    CREATE DATA table-modified_data_ext TYPE HANDLE table_descr.

    DATA(execute_default_select) = abap_true.
    "---EXTENSION CALL---
    config-ext-data->default_select( CHANGING execute = execute_default_select ).

    IF execute_default_select = abap_true.
      "---SELECTION---
      DATA(where) = table-selection->get_where_cond( ).
      SELECT * FROM (config-table_name) WHERE (where) ORDER BY PRIMARY KEY INTO TABLE @<initial_data>.
      "TODO maintenance view
    ENDIF.

    "---EXTENSION CALL---
    config-ext-data->initial_data( CHANGING initial_data = table-initial_data ).
  ENDMETHOD.


  METHOD remove_empty_rows.
    "Build where clause
    DATA(fc) = table-fields->get_fc_with_add_fields( table-additional_fields ).
    DATA(where) = ||.
    LOOP AT fc REFERENCE INTO DATA(field).
      where = |{ where } { field->fieldname  } IS INITIAL AND|.
    ENDLOOP.
    where = substring( val = where len = strlen( where ) - 4 ).
    "Remove empty rows
    assign_to_table_fs table-modified_data_ext->* <modified_data_ext>.
    DATA(lines_before) = lines( <modified_data_ext> ).
    DELETE <modified_data_ext> WHERE (where).

    IF lines_before <> lines(  <modified_data_ext> ).
      grid->refresh_table_display( ).
    ENDIF.
  ENDMETHOD.


  METHOD setup_grid.
    grid->register_edit_event( cl_gui_alv_grid=>mc_evt_modified ). "Allows to catch edit events
    grid->register_edit_event( cl_gui_alv_grid=>mc_evt_enter ). "Allows also to catch Enter

    SET HANDLER on_data_changed FOR grid.
    SET HANDLER config-ext-data->on_data_changed FOR grid.
    SET HANDLER config-ext-data->on_data_changed_finished FOR grid.

    "---EXTENSION CALL---
    config-ext-config->grid_setup( CHANGING grid = grid ).
  ENDMETHOD.


  METHOD zif_zabap_table_edit_tab_data~get_selected_row_key.
    DATA(selected) = get_selected_row_index( ).
    IF selected = 0.
      RETURN.
    ENDIF.

    "Build and fill tabkey of selected record
    assign_to_table_fs table-modified_data_ext->* <modified_data_ext>.

    "Create key struct to cast to cdtabkey - needed to extract just key fields
    table-fields->get_keys_structure( EXPORTING include_index_field = abap_false IMPORTING struct = DATA(key_struct) ).
    DATA key_line TYPE REF TO data.
    CREATE DATA key_line TYPE HANDLE key_struct.
    FIELD-SYMBOLS <key_line> TYPE any.
    ASSIGN key_line->* TO <key_line>.

    <key_line> = CORRESPONDING #( <modified_data_ext>[ selected ] ).
    tabkey = <key_line>.
  ENDMETHOD.


  METHOD zif_zabap_table_edit_tab_data~lock_table.
    IF table-locker->lock_table( IMPORTING error_message = error_message ) = abap_false.
      RETURN.
    ENDIF.

    "---TEXT TABLE---
    IF table-text_table->lock_table( IMPORTING error_message = error_message ) = abap_false.
      table-locker->unlock_table( ).
      RETURN.
    ENDIF.

    locked = abap_true.
  ENDMETHOD.


  METHOD zif_zabap_table_edit_tab_data~reset_grid.
    was_data_changed = abap_false.

    "You have declare <fs> type table, and assign dynamic table, because you can't just use corresponding with table-initial_data->* :(
    assign_to_table_fs table-initial_data->* <initial_data>.
    assign_to_table_fs table-modified_data_ext->* <modified_data_ext>.
    <modified_data_ext> = CORRESPONDING #( <initial_data> ).

    "---TEXT TABLE---
    table-text_table->update_text_elements( CHANGING extended = table-modified_data_ext ).

    table-fields->set_edit_mode( in_edit_mode ).
    DATA(fc) = table-fields->get_fc_with_add_fields( table-additional_fields ).

    "---EXTENSION CALL---
    config-ext-data->refresh_grid( EXPORTING in_edit_mode = in_edit_mode
        CHANGING field_catalogue = fc initial_data = table-initial_data modified_data_ext = table-modified_data_ext ).

    DATA(field_cat) = CORRESPONDING lvc_t_fcat( fc ).
    grid->set_table_for_first_display( EXPORTING is_variant = VALUE #( report = config-table_name handle = 'BASE' username = sy-uname )
                                                 is_layout = VALUE #( sel_mode = 'A' ) i_save = 'A'
                                       CHANGING it_outtab = <modified_data_ext> it_fieldcatalog = field_cat ).

    grid->set_ready_for_input( COND #( WHEN in_edit_mode = abap_true THEN 1 ELSE 0 ) ).
  ENDMETHOD.


  METHOD zif_zabap_table_edit_tab_data~restrict_selection.
    changed = table-selection->display( was_data_changed ).
    IF changed = abap_true.
      prepare_initial_data( ).
    ENDIF.
  ENDMETHOD.


  METHOD zif_zabap_table_edit_tab_data~save_data.
    TRY.
        "---EXTENSION CALL---
        config-ext-data->before_save( CHANGING compared = compared ).

        assign_to_table_fs compared-modified->* <modified>.
        assign_to_table_fs compared-inserted->* <inserted>.
        assign_to_table_fs compared-deleted->* <deleted>.

        "Actual db changes
        table-db->delete_data( table = config-table_name table_data = <deleted> ).
        IF table-fields->key_fields_only = abap_true.
          "^Can't use modify if all fields are key fields. Also in this case it's impossible to have modified entries.
          table-db->insert_data( table = config-table_name table_data = <inserted> ).
        ELSE.
          table-db->modify_data( table = config-table_name table_data = <modified> ).
          table-db->modify_data( table = config-table_name table_data = <inserted> ).
        ENDIF.

        "Change doc creation
        IF config-change_doc_type <> space.
          create_change_doc( compared ).
        ENDIF.

        "---TEXT TABLE---
        table-text_table->save( initial = table-initial_data extended = table-modified_data_ext ).

        "Commmit all and check for errors
        COMMIT WORK AND WAIT.
        IF sy-subrc <> 0.
          MESSAGE e014(zabap_table_edit) WITH sy-subrc INTO DATA(msg).
          RAISE EXCEPTION TYPE zcx_zabap_table_edit EXPORTING custom_message = msg.
        ENDIF.

        "---EXTENSION CALL---
        config-ext-data->after_save( CHANGING compared = compared ).

        prepare_initial_data( ).
        sucess = abap_true.

      CATCH zcx_zabap_table_edit INTO DATA(zcx).
        sucess = abap_false.
        ROLLBACK WORK.
        erorr_message = zcx->get_text( ).
    ENDTRY.
  ENDMETHOD.


  METHOD zif_zabap_table_edit_tab_data~unlock_table.
    table-locker->unlock_table( ).
    table-text_table->unlock_table( ).
  ENDMETHOD.


  METHOD zif_zabap_table_edit_tab_data~validate.
    grid->check_changed_data( IMPORTING e_valid = DATA(valid) ).
    IF valid = abap_false.
      result = zcl_zabap_table_edit_globals=>c_validation-incorrect_values.
      RETURN.
    ENDIF.

    remove_empty_rows( ).
    table-comparator->update_mandant( table-modified_data_ext ).
    DATA(modified_data) = get_modified_data_no_ext( ).

    table-comparator->compare_tables( EXPORTING initial_data = table-initial_data modified_data = modified_data
        IMPORTING duplicates = compared-duplicates inserted = compared-inserted deleted = compared-deleted
                  before_modified = compared-before_modified modified = compared-modified ).

    assign_to_table_fs compared-duplicates->* <duplicates>.
    IF lines( <duplicates> ) > 0.
      result = zcl_zabap_table_edit_globals=>c_validation-duplicates.
      RETURN.
    ENDIF.

    compared-not_in_selection = get_not_in_selection( compared ).
    assign_to_table_fs compared-not_in_selection->* <not_in_selection>.
    IF lines( <not_in_selection> ) > 0.
      result = zcl_zabap_table_edit_globals=>c_validation-not_in_selection.
      RETURN.
    ENDIF.

    result = zcl_zabap_table_edit_globals=>c_validation-ok.

    "---EXTENSION CALL---
    config-ext-data->additional_validation( CHANGING result = result all_modified_data = modified_data compared = compared ).
  ENDMETHOD.
ENDCLASS.
