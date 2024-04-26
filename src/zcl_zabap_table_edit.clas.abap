CLASS zcl_zabap_table_edit DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    METHODS:
      "! @parameter table_name | <p class="shorttext synchronized">Must be valid DDIC transparent table</p>
      "! @parameter extension_inst | <p class="shorttext synchronized">instance of a class implementing ZIF_ZABAP_TABLE_EDIT</p>
      "! @raising cx_sy_create_object_error | <p class="shorttext synchronized">If there is error when creating class of supplied name </p>
      constructor IMPORTING table_name TYPE string extension_inst TYPE REF TO zif_zabap_table_edit OPTIONAL header_text TYPE string DEFAULT '' RAISING cx_sy_create_object_error,
      set_change_doc_type IMPORTING change_doc_type TYPE zabap_change_doc_type,
      set_edit_mode IMPORTING editable TYPE abap_bool,
      display.

  PRIVATE SECTION.
    CONSTANTS:
      BEGIN OF c_validation,
        incorrect_values TYPE i VALUE 0,
        duplicates       TYPE i VALUE 1,
        ok               TYPE i VALUE 2,
      END OF c_validation.

    METHODS:
      "! <p class="shorttext synchronized">Create object and hook up grid events</p>
      setup_extension IMPORTING extension_inst TYPE REF TO zif_zabap_table_edit,
      "! <p class="shorttext synchronized">Initial query from specified table</p>
      prepare_initial_data,
      "! <p class="shorttext synchronized">Recreate table and field catalogue for grid</p>
      reset_grid,
      "! <p class="shorttext synchronized">Compare table and check fields with checktables</p>
      "! @parameter result |<p class="shorttext synchronized">Of type <em>c_validation</em></p>
      validate EXPORTING result TYPE i duplicates TYPE REF TO data inserted TYPE REF TO data deleted TYPE REF TO data
                               before_modified TYPE REF TO data modified TYPE REF TO data,
      create_change_doc IMPORTING inserted TYPE REF TO data deleted TYPE REF TO data
                               before_modified TYPE REF TO data modified TYPE REF TO data
                        RAISING zcx_zabap_table_edit,
      "! <p class="shorttext synchronized">Display table to original tab - needed if fields were added</p>
      get_modified_data_no_ext RETURNING VALUE(modified_data) TYPE REF TO data,
      get_selected_row_index RETURNING VALUE(index) TYPE i.

    METHODS:
      command_validate,
      command_save,
      commad_toggle_display,
      commad_change_document.
    METHODS:
      on_user_command FOR EVENT on_user_command OF zcl_zabap_screen_with_containe IMPORTING command,
      on_data_changed FOR EVENT data_changed OF cl_gui_alv_grid IMPORTING er_data_changed e_onf4 e_onf4_before e_onf4_after e_ucomm sender.

    DATA:
      messages        TYPE REF TO zcl_zabap_table_edit_messages,
      table_fields    TYPE REF TO zcl_zabap_table_fields,
      screen_controls TYPE REF TO zcl_zabap_table_edit_screen,
      table_locker    TYPE REF TO zcl_zabap_table_edit_lock,
      comparator      TYPE REF TO zcl_zabap_table_comparator,
      grid            TYPE REF TO cl_gui_alv_grid,
      extension       TYPE REF TO zif_zabap_table_edit.
    DATA:
      in_edit_mode    TYPE abap_bool VALUE abap_false,
      table_name      TYPE string,
      change_doc_type TYPE zabap_change_doc_type,
      header_text     TYPE string.
    DATA:
      additional_fields TYPE cl_abap_structdescr=>component_table,
      initial_data      TYPE REF TO data,
      "! <p class="shorttext synchronized">Original tab + add. fields. Displayed on screen in grid.</p>
      modified_data_ext TYPE REF TO data,
      was_data_changed  TYPE abap_bool VALUE abap_false.

ENDCLASS.


CLASS zcl_zabap_table_edit IMPLEMENTATION.
  METHOD constructor.
    "Initialise variables/classes
    me->table_name = table_name.
    messages = NEW #( ).
    table_fields = NEW #( table_name = table_name editable = in_edit_mode ).
    screen_controls = NEW #( ).
    table_locker = NEW #( table_name = table_name ).
    comparator = NEW #( table_name = table_name ).
    "Setup grid
    grid = NEW cl_gui_alv_grid( zcl_zabap_screen_with_containe=>get_container( ) ).
    grid->register_edit_event( cl_gui_alv_grid=>mc_evt_modified ). "Allows to catch edit events
    grid->register_edit_event( cl_gui_alv_grid=>mc_evt_enter ). "Allows also to catch Enter
    "Hook up event handlers
    SET HANDLER me->on_user_command.
    SET HANDLER on_data_changed FOR grid.

    setup_extension( extension_inst ).
    "---EXTENSION CALL---
    extension->grid_setup( CHANGING grid = grid ).
    extension->additional_fields( CHANGING additional_fields = additional_fields ).

    prepare_initial_data( ).

    "Header text
    me->header_text = header_text.
    IF me->header_text = ||.
      me->header_text = table_name.
    ENDIF.
  ENDMETHOD.

  METHOD set_change_doc_type.
    me->change_doc_type = change_doc_type.
  ENDMETHOD.

  METHOD set_edit_mode.
    "---EXTENSION CALL---
    DATA(is_editable) = editable.
    extension->set_edit_mode( CHANGING editable = is_editable ).

    IF is_editable = abap_true.
      DATA msg TYPE string.
      IF table_locker->lock_table( IMPORTING error_message = msg ) = abap_false.
        MESSAGE msg TYPE 'E'.
        RETURN.
      ENDIF.
    ELSE.
      table_locker->unlock_table( ).
    ENDIF.

    in_edit_mode = is_editable.
  ENDMETHOD.

  METHOD display.
    screen_controls->update_screen_controls( in_edit_mode ).
    "---EXTENSION CALL---
    extension->change_commands( in_edit_mode = in_edit_mode ).

    reset_grid( ).

    zcl_zabap_screen_with_containe=>display( header_text = me->header_text ).
  ENDMETHOD.

  METHOD prepare_initial_data.
    CREATE DATA initial_data TYPE TABLE OF (table_name).
    table_fields->get_table_with_add_fields( EXPORTING additional_fields = additional_fields IMPORTING table = DATA(table) ).
    CREATE DATA modified_data_ext TYPE HANDLE table.

    FIELD-SYMBOLS <initial_data> TYPE table.
    ASSIGN initial_data->* TO <initial_data>.

    "---EXTENSION CALL---
    IF NOT extension->disable_default_select( ).
      SELECT * FROM (table_name) INTO TABLE @<initial_data>
      ORDER BY PRIMARY KEY.
    ENDIF.

    "---EXTENSION CALL---
    extension->initial_data( CHANGING initial_data = initial_data ).
  ENDMETHOD.

  METHOD reset_grid.
    "You have declare <fs> type table, and assign dynamic table
    "Because you can't just e.g append lines to modified_data->*.
    FIELD-SYMBOLS <initial_data> TYPE table.
    FIELD-SYMBOLS <modified_data_ext> TYPE table.

    ASSIGN initial_data->* TO <initial_data>.
    ASSIGN modified_data_ext->* TO <modified_data_ext>.

    <modified_data_ext> = CORRESPONDING #( <initial_data> ).

    was_data_changed = abap_false.

    table_fields->set_edit_mode( in_edit_mode ).
    DATA(fc) = table_fields->get_fc_with_add_fields( additional_fields ).

    "---EXTENSION CALL---
    extension->refresh_grid( EXPORTING in_edit_mode      = in_edit_mode
                             CHANGING  field_catalogue   = fc
                                       header_text       = me->header_text
                                       initial_data      = initial_data
                                       modified_data_ext = modified_data_ext ).

    DATA(field_cat) = CORRESPONDING lvc_t_fcat( fc ).
    grid->set_table_for_first_display( EXPORTING is_variant = VALUE #( report = table_name handle = 'BASE' username = sy-uname )
                                                 is_layout = VALUE #( sel_mode = 'A' ) i_save = 'A'
                                       CHANGING it_outtab = <modified_data_ext> it_fieldcatalog = field_cat ).
  ENDMETHOD.

  METHOD validate.
    grid->check_changed_data( IMPORTING e_valid = DATA(valid) ).
    IF valid = abap_false.
      result = c_validation-incorrect_values.
      RETURN.
    ENDIF.

    comparator->update_mandant( modified_data_ext ).
    DATA(modified_data) = get_modified_data_no_ext( ).
    comparator->compare_tables( EXPORTING initial_data    = initial_data
                                          modified_data   = modified_data
                                IMPORTING duplicates      = duplicates
                                          inserted        = inserted
                                          deleted         = deleted
                                          before_modified = before_modified
                                          modified        = modified ).

    FIELD-SYMBOLS <duplicates>      TYPE table.
    ASSIGN duplicates->* TO <duplicates>.
    IF lines( <duplicates> ) > 0.
      result = c_validation-duplicates.
      RETURN.
    ENDIF.

    result = c_validation-ok.

    "---EXTENSION CALL---
    extension->additional_validation( CHANGING result            = result
                                               all_modified_data = modified_data
                                               duplicates        = duplicates
                                               inserted          = inserted
                                               deleted           = deleted
                                               before_modified   = before_modified
                                               modified          = modified ).
  ENDMETHOD.

  METHOD command_save.
    TRY.
        validate( IMPORTING result          = DATA(result)
                            duplicates      = DATA(duplicates)
                            inserted        = DATA(inserted)
                            deleted         = DATA(deleted)
                            before_modified = DATA(before_modified)
                            modified        = DATA(modified) ).

        "Abort with message if data is invalid
        IF result = c_validation-incorrect_values.
          "^Message already displayed by ALV GRID
          RETURN.
        ELSEIF result = c_validation-duplicates.
          messages->show_duplicates( table_name = table_name duplicates = duplicates mandant_col_name = table_fields->mandant_field ).
          RETURN.
        ENDIF.

        IF messages->confirm_save( ) = abap_false.
          RETURN.
        ENDIF.

        "---EXTENSION CALL---
        extension->before_save( CHANGING inserted = inserted deleted = deleted before_modified = before_modified modified = modified ).

        "Again with unnecessary "FieLDs-sYmBOls"
        FIELD-SYMBOLS <modified>        TYPE table.
        FIELD-SYMBOLS <inserted>        TYPE table.
        FIELD-SYMBOLS <deleted>         TYPE table.
        ASSIGN modified->* TO <modified>.
        ASSIGN inserted->* TO <inserted>.
        ASSIGN deleted->* TO <deleted>.
        "Actual db changes
        DELETE (table_name) FROM TABLE @<deleted>.
        IF table_fields->key_fields_only = abap_true.
          "^Can't use modify if all fields are key fields. Also in this case it's impossible to have modified entries.
          INSERT (table_name) FROM TABLE @<inserted> ACCEPTING DUPLICATE KEYS.

        ELSE.
          MODIFY (table_name) FROM TABLE @<modified>.
          MODIFY (table_name) FROM TABLE @<inserted>.

        ENDIF.

        "Change doc creation
        IF change_doc_type <> space.
          create_change_doc( inserted = inserted deleted = deleted before_modified = before_modified modified = modified ).
        ENDIF.

        "Commmit all and check for errors
        COMMIT WORK AND WAIT.
        IF sy-subrc <> 0.
          RAISE EXCEPTION TYPE zcx_zabap_table_edit EXPORTING custom_message = |SAP LUW commit returned { sy-subrc }|.
        ENDIF.

        "---EXTENSION CALL---
        extension->after_save( CHANGING inserted = inserted deleted = deleted before_modified = before_modified modified = modified ).

        messages->save_ok( ).
        prepare_initial_data( ).
        reset_grid( ).
        in_edit_mode = abap_false.
        display( ).

      CATCH zcx_zabap_table_edit INTO DATA(zcx).
        ROLLBACK WORK.
        messages->save_error( zcx->get_text( ) ).

    ENDTRY.

  ENDMETHOD.

  METHOD command_validate.
    validate( IMPORTING result = DATA(result) duplicates = DATA(duplicates) ).
    CASE result.
      WHEN c_validation-incorrect_values. "Nothing - message was already displayed by ALV GRID
      WHEN c_validation-duplicates. messages->show_duplicates( table_name = table_name duplicates = duplicates mandant_col_name = table_fields->mandant_field ).
      WHEN c_validation-ok. messages->validation_ok( ).
    ENDCASE.
  ENDMETHOD.

  METHOD commad_toggle_display.
    IF in_edit_mode = abap_true.
      IF messages->confirm_data_loss( was_data_changed ) = abap_false.
        RETURN.
      ENDIF.
    ENDIF.
    set_edit_mode( COND #( WHEN in_edit_mode = abap_true THEN abap_false ELSE abap_true ) ).
    display( ).
  ENDMETHOD.

  METHOD commad_change_document.
    DATA(selected_row) = get_selected_row_index( ).
    DATA batch_input TYPE TABLE OF bdcdata.

    APPEND VALUE #( program = 'RSSCD100' dynpro = '1000' dynbegin = 'X' fnam = 'BDC_CURSOR' fval = 'TABNAME'  ) TO batch_input.
    APPEND VALUE #( fnam = 'OBJEKT' fval = '' ) TO batch_input.
    APPEND VALUE #( fnam = 'OBJEKTID' fval = table_name ) TO batch_input.
    APPEND VALUE #( fnam = 'TABNAME' fval = table_name ) TO batch_input.
    IF selected_row > 0.
      "Build and fill tabkey of selected record
      FIELD-SYMBOLS <modified_data_ext> TYPE table.
      ASSIGN modified_data_ext->* TO <modified_data_ext>.

      "Create key struct to cast to cdtabkey - needed to extract just key fields
      table_fields->get_keys_structure( EXPORTING include_index_field = abap_false IMPORTING struct = DATA(key_struct) ).
      DATA key_line TYPE REF TO data.
      CREATE DATA key_line TYPE HANDLE key_struct.
      FIELD-SYMBOLS <key_line> TYPE any.
      ASSIGN key_line->* TO <key_line>.

      <key_line> = CORRESPONDING #( <modified_data_ext>[ selected_row  ] ).
      DATA tabkey TYPE cdtabkey.
      tabkey = <key_line>.

      APPEND VALUE #( fnam = 'TABKEY' fval = tabkey ) TO batch_input.

    ELSE.
      "Clear in case of leftovers from previous call
      APPEND VALUE #( fnam = 'TABKEY' fval = '' ) TO batch_input.

    ENDIF.

    CALL TRANSACTION 'RSSCD100' USING batch_input MODE 'E' UPDATE 'S'.
  ENDMETHOD.

  METHOD on_user_command.
    "---EXTENSION CALL---
    DATA(cancel_command) = abap_false.
    extension->before_command( CHANGING command = command cancel_command = cancel_command ).
    IF cancel_command = abap_true. RETURN. ENDIF.

    CASE command.
      WHEN 'SAVE'.
        command_save( ).

      WHEN 'TOGGLE_DISPLAY'.
        commad_toggle_display( ).

      WHEN 'VALIDATE'.
        command_validate( ).

      WHEN 'CHANGE_DOCUMENT'.
        commad_change_document( ).

      WHEN 'RESET'.
        IF messages->confirm_data_loss( was_data_changed ) = abap_false.
          RETURN.
        ENDIF.
        reset_grid( ).

      WHEN 'BACK' OR 'EXIT'.
        IF messages->confirm_data_loss( was_data_changed ) = abap_false.
          RETURN.
        ENDIF.
        LEAVE TO SCREEN 0.

      WHEN 'CANCEL'.
        IF messages->confirm_data_loss( was_data_changed ) = abap_false.
          RETURN.
        ENDIF.
        LEAVE PROGRAM.

    ENDCASE.

    "---EXTENSION CALL---
    extension->after_command( CHANGING command = command ).
  ENDMETHOD.



  METHOD create_change_doc.
    DATA(cd) = NEW zcl_zabap_change_document( objectclass = CONV #( table_name ) objectid = CONV #( table_name ) ).

    cd->open( ).
    cd->change_multi( EXPORTING force_cd_on_all_fields = COND #( WHEN change_doc_type = 'F' THEN abap_true ELSE abap_false )
                                 table_name = table_name
                                 deleted = cd->create_table_with_indicator( table_name = table_name original_table = deleted indicator = 'D' )
                                 inserted = cd->create_table_with_indicator( table_name = table_name original_table = inserted indicator = 'I' )
                                 before_modified = cd->create_table_with_indicator( table_name = table_name original_table = before_modified indicator = 'U' )
                                 modified = cd->create_table_with_indicator( table_name = table_name original_table = modified  ) ).
    "Some SAP magic to get initial t-code
    DATA: original_tcode TYPE sytcode.
    CALL 'GET_PARAM_TCOD' ID 'PTCOD' FIELD original_tcode.
    cd->close( tcode = original_tcode ).
  ENDMETHOD.


  METHOD setup_extension.
    IF extension_inst IS BOUND.
      extension = extension_inst.
    ELSE.
      extension = NEW zcl_zabap_table_edit_empty_if(  ).
    ENDIF.

    SET HANDLER extension->on_data_changed FOR grid.
    SET HANDLER extension->on_data_changed_finished FOR grid.
  ENDMETHOD.

  METHOD on_data_changed.
    was_data_changed = abap_true.
  ENDMETHOD.

  METHOD get_modified_data_no_ext.
    CREATE DATA modified_data TYPE TABLE OF (table_name).

    FIELD-SYMBOLS <modified_data> TYPE table.
    FIELD-SYMBOLS <modified_data_ext> TYPE table.

    ASSIGN modified_data->* TO <modified_data>.
    ASSIGN modified_data_ext->* TO <modified_data_ext>.

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
ENDCLASS.
