CLASS zcl_zabap_table_edit_tab_data DEFINITION PUBLIC FINAL CREATE PUBLIC.

  PUBLIC SECTION.
    TYPES:
      BEGIN OF t_config,
        table_name         TYPE  string, "TODO tabname
        change_doc_type    TYPE zabap_change_doc_type,
        disable_text_table TYPE abap_bool,
        BEGIN OF ext,
          commands TYPE REF TO zif_zabap_table_edit_commands,
          config   TYPE REF TO zif_zabap_table_edit_config,
          data     TYPE REF TO zif_zabap_table_edit_data,
        END OF ext,
      END OF t_config.

    METHODS:
      constructor IMPORTING configuration TYPE t_config,
      lock_table EXPORTING error_message TYPE string RETURNING VALUE(locked) TYPE abap_bool,
      unlock_table,
      "! <p class="shorttext synchronized">Recreate table and field catalogue for grid</p>
      "! @parameter in_edit_mode | <p class="shorttext synchronized"></p>
      reset_grid IMPORTING in_edit_mode TYPE abap_bool,
      "! <p class="shorttext synchronized">Compare table and check fields with checktables</p>
      "! @parameter result |<p class="shorttext synchronized">Of type <em>c_validation</em></p>
      validate EXPORTING result TYPE i compared TYPE zcl_zabap_table_edit_globals=>t_data_comparision,
      save_data EXPORTING erorr_message TYPE string CHANGING compared TYPE zcl_zabap_table_edit_globals=>t_data_comparision
                RETURNING VALUE(sucess) TYPE abap_bool,
      get_selected_row_key RETURNING VALUE(tabkey) TYPE string.

    DATA:
      mandant_field    TYPE string READ-ONLY,
      was_data_changed TYPE abap_bool VALUE abap_false READ-ONLY.

  PRIVATE SECTION.
    TYPES:
      BEGIN OF t_table,
        initial_data      TYPE REF TO data,
        additional_fields TYPE cl_abap_structdescr=>component_table,
        "! <p class="shorttext synchronized" lang="en">Original tab + add. fields. Displayed on screen in grid.</p>
        modified_data_ext TYPE REF TO data,
        fields            TYPE REF TO zcl_zabap_table_fields,
        locker            TYPE REF TO zcl_zabap_table_edit_lock,
        comparator        TYPE REF TO zcl_zabap_table_comparator,
        text_table        TYPE REF TO zif_zabap_table_edit_text_tab,
        maintenance_view  TYPE REF TO zif_zabap_table_edit_text_tab,

        db                TYPE REF TO zif_zabap_table_edit_db,
      END OF t_table.

    METHODS:
      setup_grid,
      setup_maintenance_view,
      "! <p class="shorttext synchronized">Initial query from specified table</p>
      prepare_initial_data,
      create_change_doc IMPORTING compared TYPE zcl_zabap_table_edit_globals=>t_data_comparision  RAISING zcx_zabap_table_edit,
      "! <p class="shorttext synchronized">Display table to original tab - needed if fields were added</p>
      get_modified_data_no_ext RETURNING VALUE(modified_data) TYPE REF TO data,
      get_selected_row_index RETURNING VALUE(index) TYPE i.

    METHODS:
      on_data_changed FOR EVENT data_changed OF cl_gui_alv_grid IMPORTING er_data_changed e_onf4 e_onf4_before e_onf4_after e_ucomm sender.

    DATA:
      table    TYPE t_table,
      grid     TYPE REF TO cl_gui_alv_grid,
      messages TYPE REF TO zcl_zabap_table_edit_messages.

    DATA:
      config          TYPE t_config.
ENDCLASS.


CLASS zcl_zabap_table_edit_tab_data IMPLEMENTATION.
  METHOD constructor.
    config = configuration.

    table-fields     = NEW #( table_name = config-table_name ). "TODO as interface
    table-locker     = NEW #( table_name = config-table_name ). "TODO as interface
    table-comparator = NEW #( table_name = config-table_name ). "TODO as interface

    "---TEXT TABLE---
    table-text_table = zcl_zabap_table_edit_factory=>get_text_table( CORRESPONDING #( me->config ) ).
    table-text_table->append_additional_fields( CHANGING additional_fields = table-additional_fields ).

*    "---MAINTANANCE VIEW---
*    setup_maintenance_view( ).

    setup_grid( ).

    "---EXTENSION CALL---
    config-ext-data->additional_fields( CHANGING additional_fields = table-additional_fields ).

    prepare_initial_data( ).
    table-db = zcl_zabap_table_edit_factory=>get_db( ).
  ENDMETHOD.
  METHOD setup_grid.
    grid = NEW cl_gui_alv_grid( zcl_zabap_screen_with_containe=>get_container( ) ).
    grid->register_edit_event( cl_gui_alv_grid=>mc_evt_modified ). "Allows to catch edit events
    grid->register_edit_event( cl_gui_alv_grid=>mc_evt_enter ). "Allows also to catch Enter

    SET HANDLER on_data_changed FOR grid.
    SET HANDLER config-ext-data->on_data_changed FOR grid.
    SET HANDLER config-ext-data->on_data_changed_finished FOR grid.

    "---EXTENSION CALL---
    config-ext-config->grid_setup( CHANGING grid = grid ).
  ENDMETHOD.

  METHOD setup_maintenance_view.
    "TODO

*    text_table = NEW zcl_zabap_table_edit_text_tab( original_table = table_name change_doc_type = change_doc_type ).
*    text_table->append_additional_fields( CHANGING additional_fields = additional_fields ).
  ENDMETHOD.



  METHOD prepare_initial_data.
    "TODO   maint view?
    CREATE DATA table-initial_data TYPE TABLE OF (config-table_name).
    assign_to_table_fs table-initial_data->* <initial_data>.

    table-fields->get_table_with_add_fields( EXPORTING additional_fields = table-additional_fields IMPORTING table = DATA(table_descr) ).
    CREATE DATA table-modified_data_ext TYPE HANDLE table_descr.

    DATA(execute_default_select) = abap_true.
    "---EXTENSION CALL---
    config-ext-data->default_select( CHANGING execute = execute_default_select ).

    IF execute_default_select = abap_true.
      SELECT * FROM (config-table_name) INTO TABLE @<initial_data> ORDER BY PRIMARY KEY.
      "TODO maintenance view
    ENDIF.

    "---EXTENSION CALL---
    config-ext-data->initial_data( CHANGING initial_data = table-initial_data ).
  ENDMETHOD.

  METHOD lock_table.
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

  METHOD unlock_table.
    table-locker->unlock_table( ).
    table-text_table->unlock_table( ).
  ENDMETHOD.

  METHOD reset_grid.
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
  ENDMETHOD.

  METHOD validate.
    grid->check_changed_data( IMPORTING e_valid = DATA(valid) ).
    IF valid = abap_false.
      result = zcl_zabap_table_edit_globals=>c_validation-incorrect_values.
      RETURN.
    ENDIF.

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

    result = zcl_zabap_table_edit_globals=>c_validation-ok.

    "---EXTENSION CALL---
    config-ext-data->additional_validation( CHANGING result = result all_modified_data = modified_data compared = compared ).
  ENDMETHOD.

  METHOD save_data.
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
          RAISE EXCEPTION TYPE zcx_zabap_table_edit EXPORTING custom_message = |SAP LUW commit returned { sy-subrc }|.
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

  METHOD create_change_doc.
    DATA(cd) = ZCL_ZABAP_TABLE_EDIT_FACTORY=>get_change_doc( objectclass = CONV #( config-table_name ) objectid = CONV #( config-table_name ) ).

    cd->open( ).
    cd->change_multi( force_cd_on_all_fields = COND #( WHEN config-change_doc_type = 'F' THEN abap_true ELSE abap_false )
                       table_name = config-table_name
                       deleted = cd->create_table_with_indicator( table_name = config-table_name original_table = compared-deleted indicator = 'D' )
                       inserted = cd->create_table_with_indicator( table_name = config-table_name original_table = compared-deleted  indicator = 'I' )
                       before_modified = cd->create_table_with_indicator( table_name = config-table_name original_table = compared-deleted  indicator = 'U' )
                       modified = cd->create_table_with_indicator( table_name = config-table_name original_table = compared-deleted  ) ).

    "Some SAP magic to get initial t-code
    DATA: original_tcode TYPE sytcode.
    CALL 'GET_PARAM_TCOD' ID 'PTCOD' FIELD original_tcode.
    cd->close( tcode = original_tcode ).
  ENDMETHOD.

  METHOD on_data_changed.
    was_data_changed = abap_true.
  ENDMETHOD.

  METHOD get_modified_data_no_ext.
    CREATE DATA modified_data TYPE TABLE OF (config-table_name).

    assign_to_table_fs modified_data->* <modified_data>.
    assign_to_table_fs table-modified_data_ext->* <modified_data_ext>.

    <modified_data> = CORRESPONDING #( <modified_data_ext> ).
  ENDMETHOD.

  METHOD get_selected_row_index.
    grid->get_selected_rows( IMPORTING et_index_rows = DATA(selected_rows) ).
    IF lines( selected_rows ) = 1.
      index = selected_rows[ 1 ]-index.
      RETURN.
    ENDIF.

    grid->get_selected_cells( IMPORTING et_cell = DATA(selected_cells) ).
    IF lines( selected_cells ) = 1.
      index = selected_cells[ 1 ]-row_id.
      RETURN.
    ENDIF.
  ENDMETHOD.

  METHOD get_selected_row_key.
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
    "TODO check with decimals
    <key_line> = CORRESPONDING #( <modified_data_ext>[ selected ] ).
    tabkey = <key_line>.
*    DATA cdtabkeylo TYPE cdtabkeylo.
*    cdtabkeylo = <key_line>.
*    "Can't assign directly to string
*    tabkey = cdtabkeylo.
  ENDMETHOD.
ENDCLASS.
