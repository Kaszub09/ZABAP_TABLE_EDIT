CLASS zcl_zabap_table_edit_tab_data DEFINITION PUBLIC FINAL CREATE PRIVATE GLOBAL FRIENDS zcl_zabap_table_edit_factory.

  PUBLIC SECTION.
    INTERFACES:
      zif_zabap_table_edit_tab_data.

    ALIASES:
     mandant_field FOR zif_zabap_table_edit_tab_data~mandant_field,
     was_data_changed FOR zif_zabap_table_edit_tab_data~was_data_changed .

    METHODS:
      constructor IMPORTING configuration TYPE zif_zabap_table_edit_tab_data=>t_config grid TYPE REF TO zif_zabap_table_edit_grid_if.

  PRIVATE SECTION.
    TYPES:
      BEGIN OF t_table,
        name              TYPE string,
        initial_data      TYPE REF TO data,
        additional_fields TYPE cl_abap_structdescr=>component_table,
        "! <p class="shorttext synchronized">Original tab + add. fields. Displayed on screen in grid.</p>
        modified_data_ext TYPE REF TO data,
        fields            TYPE REF TO zcl_zabap_table_fields,
        locker            TYPE REF TO zcl_zabap_table_edit_lock,
        comparator        TYPE REF TO zcl_zabap_table_comparator,
        text_table        TYPE REF TO zif_zabap_table_edit_text_tab,
        view              TYPE REF TO zif_zabap_table_edit_mview,
        db                TYPE REF TO zif_zabap_table_edit_db,
      END OF t_table.

    METHODS:
      setup_grid,
      "! <p class="shorttext synchronized">Initial query from specified table</p>
      prepare_initial_data,
      create_change_doc IMPORTING compared TYPE zcl_zabap_table_edit_globals=>t_data_comparision  RAISING zcx_zabap_table_edit,
      "! <p class="shorttext synchronized">Display table to original tab - needed if fields were added</p>
      get_modified_data_no_ext RETURNING VALUE(modified_data) TYPE REF TO data,
      get_selected_row_index RETURNING VALUE(index) TYPE i.

    METHODS:
      on_data_changed FOR EVENT data_changed OF zif_zabap_table_edit_grid_if IMPORTING er_data_changed e_onf4 e_onf4_before e_onf4_after e_ucomm sender.

    DATA:
      table    TYPE t_table,
      grid     TYPE REF TO zif_zabap_table_edit_grid_if,
      messages TYPE REF TO zcl_zabap_table_edit_messages.

    DATA:
      config          TYPE zif_zabap_table_edit_tab_data=>t_config.
ENDCLASS.

CLASS zcl_zabap_table_edit_tab_data IMPLEMENTATION.
  METHOD constructor.
    config = configuration.

    "---MAINT VIEW---
    table-view = zcl_zabap_table_edit_factory=>get_view( config-view_name ).
    table-name = table-view->base_table.

    table-fields     = NEW #( table_name = table-name ).
    table-locker     = NEW #( table_name = table-name ).
    table-comparator = NEW #( table_name = table-name ).

    "---TEXT TABLE---
    table-text_table = zcl_zabap_table_edit_factory=>get_text_table( VALUE #( BASE CORRESPONDING #( me->config ) table_name = table-name ) ).
    table-text_table->append_additional_fields( CHANGING additional_fields = table-additional_fields ).

    me->grid = grid.
    setup_grid( ).

    "---EXTENSION CALL---
    config-ext-data->additional_fields( CHANGING additional_fields = table-additional_fields ).

    prepare_initial_data( ).
    table-db = zcl_zabap_table_edit_factory=>get_db( ).
  ENDMETHOD.

  METHOD create_change_doc.
    DATA(cd) = zcl_zabap_table_edit_factory=>get_change_doc( objectclass = CONV #( table-name ) objectid = CONV #( table-name ) ).

    cd->open( ).
    cd->change_multi( force_cd_on_all_fields = COND #( WHEN config-change_doc_type = 'F' THEN abap_true ELSE abap_false )
                       table_name = table-name
                       deleted = cd->create_table_with_indicator( table_name = table-name original_table = compared-deleted indicator = 'D' )
                       inserted = cd->create_table_with_indicator( table_name = table-name original_table = compared-inserted  indicator = 'I' )
                       before_modified = cd->create_table_with_indicator( table_name = table-name original_table = compared-before_modified  indicator = 'U' )
                       modified = cd->create_table_with_indicator( table_name = table-name original_table = compared-modified  ) ).

    "Some SAP magic to get initial t-code
    DATA: original_tcode TYPE sytcode.
    CALL 'GET_PARAM_TCOD' ID 'PTCOD' FIELD original_tcode.
    cd->close( tcode = original_tcode ).
  ENDMETHOD.

  METHOD get_modified_data_no_ext.
    CREATE DATA modified_data TYPE TABLE OF (table-name).

    assign_to_table_fs modified_data->* <modified_data>.
    assign_to_table_fs table-modified_data_ext->* <modified_data_ext>.

    "---MAINT VIEW---
    table-view->map_table_ext_view_to_base( CHANGING extended_view = <modified_data_ext> base = <modified_data> ).
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

  METHOD on_data_changed.
    was_data_changed = abap_true.
    table-view->update_view_values( er_data_changed ).
  ENDMETHOD.

  METHOD prepare_initial_data.
    CREATE DATA table-initial_data TYPE TABLE OF (table-name).
    assign_to_table_fs table-initial_data->* <initial_data>.

    "---MAINT VIEW---
    table-modified_data_ext = table-view->get_ext_view( table-additional_fields ).

    DATA(execute_default_select) = abap_true.
    "---EXTENSION CALL---
    config-ext-data->default_select( CHANGING execute = execute_default_select ).

    IF execute_default_select = abap_true.
      "---MAINT VIEW---
      table-view->run_initial_select( CHANGING initial_data = <initial_data> ).
    ENDIF.

    "---EXTENSION CALL---
    config-ext-data->initial_data( CHANGING initial_data = table-initial_data ).
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
    assign_to_table_fs table-modified_data_ext->* <modified_data_ext>.

    "Create key struct to cast to cdtabkey - needed to extract just key fields
    table-fields->get_keys_structure( EXPORTING include_index_field = abap_false IMPORTING struct = DATA(key_struct) ).
    DATA key_line TYPE REF TO data.
    CREATE DATA key_line TYPE HANDLE key_struct.
    FIELD-SYMBOLS <key_line> TYPE any.
    ASSIGN key_line->* TO <key_line>.

    "---MAINT VIEW---
    table-view->map_struct_ext_view_to_base( CHANGING extended_view = <modified_data_ext>[ selected ] base = <key_line> ).

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

    "You have to declare <fs> type table, and assign dynamic table, because you can't just use corresponding with table-initial_data->* :(
    assign_to_table_fs table-initial_data->* <initial_data>.
    assign_to_table_fs table-modified_data_ext->* <modified_data_ext>.

    "---MAINT VIEW---
    table-view->map_table_base_to_ext_view( CHANGING base = <initial_data> extended_view = <modified_data_ext>  ).
    table-view->update_non_base_fields( CHANGING extended_view = <modified_data_ext> ).

    "---TEXT TABLE---
    "Can't have Text table for maintenance view, so even if data_ext doesn't have base table fields
    "(because names are taken from view) the text table must be empty so call is safe
    table-text_table->update_text_elements( CHANGING extended = table-modified_data_ext ).

    "---MAINT VIEW---
    DATA(fc) = table-view->get_ext_view_fc_with_add_field( in_edit_mode = in_edit_mode additional_fields = table-additional_fields ).

    "---EXTENSION CALL---
    config-ext-data->refresh_grid( EXPORTING in_edit_mode = in_edit_mode
        CHANGING field_catalogue = fc initial_data = table-initial_data modified_data_ext = table-modified_data_ext ).

    DATA(field_cat) = CORRESPONDING lvc_t_fcat( fc ).
    grid->set_table_for_first_display( EXPORTING is_variant = VALUE #( report = table-name handle = 'BASE' username = sy-uname )
                                                 is_layout = VALUE #( sel_mode = 'A' ) i_save = 'A'
                                       CHANGING it_outtab = <modified_data_ext> it_fieldcatalog = field_cat ).
  ENDMETHOD.

  METHOD zif_zabap_table_edit_tab_data~save_data.
    TRY.
        "---EXTENSION CALL---
        config-ext-data->before_save( CHANGING compared = compared ).

        assign_to_table_fs compared-modified->* <modified>.
        assign_to_table_fs compared-inserted->* <inserted>.
        assign_to_table_fs compared-deleted->* <deleted>.

        "Actual db changes
        table-db->delete_data( table = table-name table_data = <deleted> ).
        IF table-fields->key_fields_only = abap_true.
          "^Can't use modify if all fields are key fields. Also in this case it's impossible to have modified entries.
          table-db->insert_data( table = table-name table_data = <inserted> ).
        ELSE.
          table-db->modify_data( table = table-name table_data = <modified> ).
          table-db->modify_data( table = table-name table_data = <inserted> ).
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

    DATA(modified_data) = get_modified_data_no_ext( ).
    table-comparator->update_mandant( modified_data ).
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

  METHOD zif_zabap_table_edit_tab_data~base_table_name.
    name = table-name.
  ENDMETHOD.

ENDCLASS.
