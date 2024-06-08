CLASS zcl_zabap_change_document DEFINITION PUBLIC CREATE PRIVATE GLOBAL FRIENDS zcl_zabap_table_edit_factory.

  PUBLIC SECTION.
    INTERFACES:
      zif_zabap_change_document.

    ALIASES:
        create_table_with_indicator FOR zif_zabap_change_document~create_table_with_indicator.

    METHODS:
      "! @parameter objectclass | <p class="shorttext synchronized">Name of CD object (e.g. from SCDO)</p>
      "! @parameter objectid |  <p class="shorttext synchronized">Object ID inside CD object, e.g. matnr for MATERIAL class...</p>
      "! Something that ties records from all tables in SCDO, like common foreign key, e.g. vbeln for ekko/ekpo
      constructor IMPORTING objectclass TYPE cdobjectcl objectid TYPE cdobjectv.

    CONSTANTS:
        c_change_doc_force_log_all_fie TYPE c LENGTH 40 VALUE 'ZABAP_CHANGE_DOC_FORCE_LOG_ALL_FIELDS'.

  PRIVATE SECTION.
    TYPES:
      BEGIN OF t_table_fields,
        table_name   TYPE string,
        table_fields TYPE REF TO zcl_zabap_table_fields,
      END OF t_table_fields,
      tt_table_fields TYPE  HASHED TABLE OF t_table_fields WITH UNIQUE KEY table_name.

    METHODS:
      create_empty_table IMPORTING table_name TYPE string RETURNING VALUE(empty_table) TYPE REF TO data,
      get_table_fields IMPORTING table_name TYPE string RETURNING VALUE(table_fields) TYPE REF TO zcl_zabap_table_fields,
      set_force_cd IMPORTING force_marker TYPE abap_bool table_name TYPE string,
      clear_force_cd IMPORTING force_marker TYPE abap_bool table_name TYPE string.

    DATA:
      all_table_fields TYPE tt_table_fields,
      objectclass      TYPE cdobjectcl,
      objectid         TYPE cdobjectv.
ENDCLASS.

CLASS zcl_zabap_change_document IMPLEMENTATION.
  METHOD constructor.
    me->objectclass = objectclass.
    me->objectid    = objectid.
  ENDMETHOD.

  METHOD zif_zabap_change_document~open.
    CALL FUNCTION 'CHANGEDOCUMENT_OPEN'
      EXPORTING
        objectclass      = objectclass
        objectid         = objectid
      EXCEPTIONS
        sequence_invalid = 1                " No CLOSE was called for last object
        OTHERS           = 2.

    IF sy-subrc <> 0.
      MESSAGE e001(zabap_change_doc) WITH sy-subrc INTO DATA(error_message).
      RAISE EXCEPTION TYPE zcx_zabap_table_edit EXPORTING custom_message = error_message.
    ENDIF.
  ENDMETHOD.

  METHOD zif_zabap_change_document~close.
    CALL FUNCTION 'CHANGEDOCUMENT_CLOSE'
      EXPORTING
        date_of_change          = date_of_change
        objectclass             = objectclass
        objectid                = objectid
        tcode                   = tcode
        time_of_change          = time_of_change
        username                = username
        object_change_indicator = object_change_indicator
      IMPORTING
        changenumber            = changenumber
      EXCEPTIONS
        header_insert_failed    = 1                " SQL error during INSET of header
        no_position_inserted    = 2                " No items were entered
        object_invalid          = 3                " OPEN was called with other object
        open_missing            = 4                " No OPEN was performed
        position_insert_failed  = 5                " G/L account number
        OTHERS                  = 6.

    IF sy-subrc <> 0 AND ( sy-subrc <> 2 OR skip_exception_if_no_changes = abap_false ).
      MESSAGE e002(zabap_change_doc) WITH sy-subrc INTO DATA(error_message).
      RAISE EXCEPTION TYPE zcx_zabap_table_edit EXPORTING custom_message = error_message.
    ENDIF.
  ENDMETHOD.

  METHOD zif_zabap_change_document~change_multi.
    TRY.
        set_force_cd( force_marker = force_cd_on_all_fields table_name = table_name ).

        "Empty since you always must pass both tables
        DATA(empty_table) = create_table_with_indicator( table_name = table_name original_table = create_empty_table( table_name ) ).
        assign_to_table_fs empty_table->* <empty>.

        "NEW ENTRY
        IF inserted IS BOUND.
          assign_to_table_fs inserted->* <inserted>.
          IF lines( <inserted> ) > 0.
            CALL FUNCTION 'CHANGEDOCUMENT_MULTIPLE_CASE2'
              EXPORTING
                change_indicator       = 'I'             " Change flag
                docu_insert            = save_fields-docu_insert
                tablename              = CONV tabname( table_name )                 " Name of the table structure of object class
                table_old              = <empty>
                table_new              = <inserted>
                docu_insert_if         = save_fields-docu_insert_if
              EXCEPTIONS
                nametab_error          = 1
                open_missing           = 2
                position_insert_failed = 3
                OTHERS                 = 4.

            IF sy-subrc <> 0.

              MESSAGE e003(zabap_change_doc) WITH sy-subrc INTO DATA(error_insert).
              RAISE EXCEPTION TYPE zcx_zabap_table_edit EXPORTING custom_message = error_insert.
            ENDIF.
          ENDIF.
        ENDIF.

        "DELETED ENTRY
        IF deleted IS BOUND.
          assign_to_table_fs deleted->* <deleted>.
          IF lines( <deleted> ) > 0.
            CALL FUNCTION 'CHANGEDOCUMENT_MULTIPLE_CASE2'
              EXPORTING
                change_indicator       = 'D'             " Change flag
                docu_delete            = save_fields-docu_delete
                tablename              = CONV tabname( table_name )                  " Name of the table structure of object class
                table_old              = <deleted>
                table_new              = <empty>
                docu_delete_if         = save_fields-docu_delete_if
              EXCEPTIONS
                nametab_error          = 1
                open_missing           = 2
                position_insert_failed = 3
                OTHERS                 = 4.

            IF sy-subrc <> 0.
              MESSAGE e004(zabap_change_doc) WITH sy-subrc INTO DATA(error_delete).
              RAISE EXCEPTION TYPE zcx_zabap_table_edit EXPORTING custom_message = error_delete.
            ENDIF.
          ENDIF.
        ENDIF.

        "MODIFIED ENTRY
        IF modified IS BOUND AND before_modified IS BOUND.
          assign_to_table_fs modified->* <modified>.
          assign_to_table_fs before_modified->* <before_modified>.
          IF lines( <modified> ) > 0.
            CALL FUNCTION 'CHANGEDOCUMENT_MULTIPLE_CASE2'
              EXPORTING
                change_indicator       = 'U'             " Change flag
                tablename              = CONV tabname( table_name )                  " Name of the table structure of object class
                table_old              = <before_modified>
                table_new              = <modified>
              EXCEPTIONS
                nametab_error          = 1
                open_missing           = 2
                position_insert_failed = 3
                OTHERS                 = 4.

            IF sy-subrc <> 0.
              MESSAGE e005(zabap_change_doc) WITH sy-subrc INTO DATA(error_change).
              RAISE EXCEPTION TYPE zcx_zabap_table_edit EXPORTING custom_message = error_change.
            ENDIF.
          ENDIF.
        ENDIF.
        clear_force_cd( force_marker = force_cd_on_all_fields table_name = table_name  ).

      CATCH cx_root INTO DATA(cx_root).
        "^Try to at least restore CD function-group if messed with earlier
        clear_force_cd( force_marker = force_cd_on_all_fields table_name = table_name  ).
        RAISE EXCEPTION TYPE zcx_zabap_table_edit EXPORTING custom_message = cx_root->get_longtext( ).

    ENDTRY.
  ENDMETHOD.

  METHOD zif_zabap_change_document~change_single.
    TRY.
        set_force_cd( force_marker = force_cd_on_all_fields table_name = table_name  ).

        "NEW ENTRY
        IF inserted IS BOUND.
          assign_to_any_fs inserted->* <inserted>.
          CALL FUNCTION 'CHANGEDOCUMENT_SINGLE_CASE'
            EXPORTING
              change_indicator       = 'I'             " Change flag
              docu_insert            = save_fields-docu_insert
              tablename              = CONV tabname( table_name )                  " Name of the table structure of object class
              workarea_new           = <inserted>
              docu_insert_if         = save_fields-docu_insert_if
            EXCEPTIONS
              nametab_error          = 1                " Error when calling NAMETAB_GET
              open_missing           = 2                " No OPEN was performed
              position_insert_failed = 3                " SQL error occurred during insert item
              OTHERS                 = 4.

          IF sy-subrc <> 0.
            MESSAGE e006(zabap_change_doc) WITH sy-subrc INTO DATA(error_insert).
            RAISE EXCEPTION TYPE zcx_zabap_table_edit EXPORTING custom_message = error_insert.
          ENDIF.

        ENDIF.

        "DELETED ENTRY
        IF deleted IS BOUND.
          assign_to_any_fs deleted->* <deleted>.
          CALL FUNCTION 'CHANGEDOCUMENT_SINGLE_CASE'
            EXPORTING
              change_indicator       = 'D'             " Change flag
              docu_delete            = save_fields-docu_delete
              tablename              = CONV tabname( table_name )                 " Name of the table structure of object class
              workarea_old           = <deleted>
              docu_delete_if         = save_fields-docu_delete_if
            EXCEPTIONS
              nametab_error          = 1                " Error when calling NAMETAB_GET
              open_missing           = 2                " No OPEN was performed
              position_insert_failed = 3                " SQL error occurred during insert item
              OTHERS                 = 4.

          IF sy-subrc <> 0.
            MESSAGE e007(zabap_change_doc) WITH sy-subrc INTO DATA(error_delete).
            RAISE EXCEPTION TYPE zcx_zabap_table_edit EXPORTING custom_message = error_delete.
          ENDIF.

        ENDIF.

        "MODIFIED ENTRY
        IF modified IS BOUND AND before_modified IS BOUND.
          assign_to_any_fs modified->* <modified>.
          assign_to_any_fs before_modified->* <before_modified>.

          CALL FUNCTION 'CHANGEDOCUMENT_SINGLE_CASE'
            EXPORTING
              change_indicator       = 'U'             " Change flag
              tablename              = CONV tabname( table_name )                  " Name of the table structure of object class
              workarea_old           = <before_modified>
              workarea_new           = <modified>
            EXCEPTIONS
              nametab_error          = 1                " Error when calling NAMETAB_GET
              open_missing           = 2                " No OPEN was performed
              position_insert_failed = 3                " SQL error occurred during insert item
              OTHERS                 = 4.

          IF sy-subrc <> 0.
            MESSAGE e008(zabap_change_doc) WITH sy-subrc INTO DATA(error_change).
            RAISE EXCEPTION TYPE zcx_zabap_table_edit EXPORTING custom_message = error_change.
          ENDIF.
        ENDIF.

        clear_force_cd( force_marker = force_cd_on_all_fields table_name = table_name ).

      CATCH cx_root INTO DATA(cx_root).
        "^Try to at least restore CD function-group if messed with earlier
        clear_force_cd( force_marker = force_cd_on_all_fields table_name = table_name ).
        RAISE EXCEPTION TYPE zcx_zabap_table_edit EXPORTING custom_message = cx_root->get_longtext( ).

    ENDTRY.
  ENDMETHOD.

  METHOD create_table_with_indicator.
    DATA(table_fields) = get_table_fields( table_name = table_name ).
    table_fields->get_keys_structure( EXPORTING include_index_field = abap_true IMPORTING index_field_name = indicator_field_name struct = DATA(key_struct) ).
    table_fields->get_table_with_add_fields(
        EXPORTING additional_fields = VALUE #( ( name = indicator_field_name type = CAST #( cl_abap_structdescr=>describe_by_name( 'CDCHNGINDH' ) ) ) )
        IMPORTING struct = DATA(struct) table = DATA(table) ).

    CREATE DATA table_with_indicator TYPE HANDLE table.

    assign_to_table_fs table_with_indicator->* <table_with_indicator>.
    assign_to_table_fs original_table->* <original_table>.
    <table_with_indicator> = CORRESPONDING #( <original_table> ).

    LOOP AT <table_with_indicator> ASSIGNING FIELD-SYMBOL(<row>).
      ASSIGN COMPONENT indicator_field_name OF STRUCTURE <row> TO FIELD-SYMBOL(<change_indicator>).
      <change_indicator> = indicator.
    ENDLOOP.

    IF sort = abap_true.
      "Create sort condition - had problems with just 'SORT <> DESCSENDING.' and decimals key fields
      DATA sort_order TYPE abap_sortorder_tab.
      LOOP AT key_struct->components REFERENCE INTO DATA(component).
        APPEND VALUE #( name = component->name descending = abap_false ) TO sort_order.
      ENDLOOP.

      SORT <table_with_indicator> BY (sort_order).
    ENDIF.
  ENDMETHOD.

  METHOD create_empty_table.
    CREATE DATA empty_table TYPE TABLE OF (table_name).
  ENDMETHOD.

  METHOD get_table_fields.
    table_fields = VALUE #( all_table_fields[ table_name = table_name ]-table_fields OPTIONAL ).
    IF NOT table_fields IS BOUND.
      INSERT VALUE #( table_name = table_name table_fields = NEW zcl_zabap_table_fields( table_name ) )
      INTO TABLE all_table_fields REFERENCE INTO DATA(new_table_fields).

      table_fields = new_table_fields->table_fields.
    ENDIF.
  ENDMETHOD.

  METHOD clear_force_cd.
    IF force_marker = abap_false.
      RETURN.
    ENDIF.

    assign_to_table_fs ('(SAPLSCD0)TABINFO') <tabinfo>.

    IF <tabinfo> IS ASSIGNED.
      DATA(where_clause) = |tabname = '{ table_name }' AND logflag = 'F'|.
      LOOP AT <tabinfo> ASSIGNING FIELD-SYMBOL(<tabinfo_row>) WHERE (where_clause).
        ASSIGN COMPONENT 'LOGFLAG' OF STRUCTURE <tabinfo_row> TO FIELD-SYMBOL(<logflag>).
        CLEAR: <logflag>.
      ENDLOOP.
    ENDIF.

    EXPORT zabap_force_logging = abap_false zabap_table_name = table_name TO MEMORY ID c_change_doc_force_log_all_fie.
  ENDMETHOD.

  METHOD set_force_cd.
    IF force_marker = abap_false.
      RETURN.
    ENDIF.

    assign_to_table_fs ('(SAPLSCD0)TABINFO') <tabinfo>.

    IF <tabinfo> IS ASSIGNED.
      DATA(where_clause) = |tabname = '{ table_name }' AND logflag = space|.
      LOOP AT <tabinfo> ASSIGNING FIELD-SYMBOL(<tabinfo_row>) WHERE (where_clause).
        ASSIGN COMPONENT 'LOGFLAG' OF STRUCTURE <tabinfo_row> TO FIELD-SYMBOL(<logflag>).
        <logflag> = 'F'.
      ENDLOOP.
    ENDIF.

    EXPORT zabap_force_logging = abap_true zabap_table_name = table_name TO MEMORY ID c_change_doc_force_log_all_fie.
  ENDMETHOD.
ENDCLASS.
