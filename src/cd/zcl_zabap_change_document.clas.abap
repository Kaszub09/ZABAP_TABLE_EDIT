CLASS zcl_zabap_change_document DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    CONSTANTS:
             c_change_doc_force_log_all_fie TYPE c LENGTH 40 VALUE 'ZABAP_CHANGE_DOC_FORCE_LOG_ALL_FIELDS'.

    METHODS:
      "! @parameter objectclass | <p class="shorttext synchronized">Name of CD object (e.g. from SCDO)</p>
      "! @parameter objectid |  <p class="shorttext synchronized">Object ID inside CD object, e.g. matnr for MATERIAL class...</p>
      "! Something that ties records from all tables in SCDO, like common doregin key, e.g. vbeln for ekko/ekpo
      constructor IMPORTING objectclass TYPE cdobjectcl objectid TYPE cdobjectv,
      "! Must be called first, before any changes
      open RAISING zcx_zabap_table_edit,
      "! <p class="shorttext synchronized">CD must be opened first</p>
      "! @parameter table_name | <p class="shorttext synchronized">Must be valid DDIC table</p>
      "! @parameter table_name | <p class="shorttext synchronized">Create CD even on Data Elements without CD logflag</p>
      "! @parameter inserted | <p class="shorttext synchronized">Must be ref to structure of type <em>table_name</em></p>
      "! @parameter deleted | <p class="shorttext synchronized">Must be ref to structure of type <em>table_name</em></p>
      "! @parameter before_modified | <p class="shorttext synchronized">Must be ref to structure of type <em>table_name</em></p>
      "! @parameter modified | <p class="shorttext synchronized">Must be ref to structure of type <em>table_name</em></p>
      change_single IMPORTING table_name TYPE string force_cd_on_all_fields TYPE abap_bool DEFAULT abap_false
                              inserted TYPE REF TO data OPTIONAL deleted TYPE REF TO data OPTIONAL
                              before_modified TYPE REF TO data OPTIONAL modified TYPE REF TO data OPTIONAL
                              RAISING zcx_zabap_table_edit,
      "! <p class="shorttext synchronized">CD must be opened first. All tables must be sorted.</p>
      "! @parameter table_name | <p class="shorttext synchronized">Create CD even on Data Elements without CD logflag</p>
      "! @parameter table_name | <p class="shorttext synchronized">Must be valid DDIC table</p>
      "! @parameter inserted | <p class="shorttext synchronized">Must be ref to table <em>table_name</em> with c field</p>
      "! @parameter deleted | <p class="shorttext synchronized">Must be ref to table <em>table_name</em> with c field</p>
      "! @parameter before_modified | <p class="shorttext synchronized">Must be ref to table <em>table_name</em> with c field</p>
      "! @parameter modified | <p class="shorttext synchronized">Must be ref to table <em>table_name</em> with c field</p>
      change_multi IMPORTING table_name TYPE string force_cd_on_all_fields TYPE abap_bool DEFAULT abap_false
                             inserted TYPE REF TO data OPTIONAL deleted TYPE REF TO data OPTIONAL
                             before_modified TYPE REF TO data OPTIONAL modified TYPE REF TO data OPTIONAL
                             RAISING zcx_zabap_table_edit,
      "! <p class="shorttext synchronized">Converts table to be used with <em>change_multi</em></p>
      "! @parameter table_name | <p class="shorttext synchronized">Must be valid DDIC table</p>
      "! @parameter original_table | <p class="shorttext synchronized">Must be ref to table <em>table_name</em></p>
      "! @parameter indicator | <p class="shorttext synchronized">Value of added c field (D, I, U or space)</p>
      "! @parameter sort | <p class="shorttext synchronized">Sort table by key fields</p>
      "! @parameter table_with_indicator | <p class="shorttext synchronized">Ref to table <em>table_name</em> with added c field</p>
      create_table_with_indicator IMPORTING table_name TYPE string original_table TYPE REF TO data indicator TYPE c DEFAULT space
                                            sort TYPE abap_bool DEFAULT abap_false
                                  RETURNING VALUE(table_with_indicator) TYPE REF TO data,
      "! <p class="shorttext synchronized">CD must be opened first</p>
      "! @parameter object_change_indicator | <p class="shorttext synchronized">For header entry only</p>
      close IMPORTING date_of_change TYPE d DEFAULT sy-datum tcode TYPE syst_tcode DEFAULT sy-tcode time_of_change TYPE t DEFAULT sy-uzeit
                      username TYPE syst_uname DEFAULT sy-uname object_change_indicator TYPE cdchngindh DEFAULT 'U'
            EXPORTING changenumber TYPE cdchangenr
            RAISING zcx_zabap_table_edit .

  PROTECTED SECTION.
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
    me->objectid = objectid.
  ENDMETHOD.


  METHOD open.
    CALL FUNCTION 'CHANGEDOCUMENT_OPEN'
      EXPORTING
        objectclass      = objectclass
        objectid         = objectid
      EXCEPTIONS
        sequence_invalid = 1                " No CLOSE was called for last object
        OTHERS           = 2.

    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE zcx_zabap_table_edit EXPORTING custom_message = |CD open error { sy-subrc }|.
    ENDIF.
  ENDMETHOD.

  METHOD close.
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
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE zcx_zabap_table_edit EXPORTING custom_message = |Close CD error - { sy-subrc }|.
    ENDIF.
  ENDMETHOD.

  METHOD change_multi.
    TRY.
        set_force_cd( force_marker = force_cd_on_all_fields table_name = table_name ).

        FIELD-SYMBOLS:
          <inserted>        TYPE table,
          <deleted>         TYPE table,
          <before_modified> TYPE table,
          <modified>        TYPE table,
          <empty>           TYPE table.

        "Empty since you always must pass both tables
        DATA(empty_table) = create_table_with_indicator( table_name = table_name original_table = create_empty_table( table_name ) ).
        ASSIGN empty_table->* TO <empty>.

        "NEW ENTRY
        IF inserted IS BOUND.
          ASSIGN inserted->* TO <inserted>.
          IF lines( <inserted> ) > 0.
            CALL FUNCTION 'CHANGEDOCUMENT_MULTIPLE_CASE2'
              EXPORTING
                change_indicator       = 'I'             " Change flag
                docu_insert            = abap_true
                tablename              = CONV tabname( table_name )                 " Name of the table structure of object class
                table_old              = <empty>
                table_new              = <inserted>
                docu_insert_if         = abap_true
              EXCEPTIONS
                nametab_error          = 1
                open_missing           = 2
                position_insert_failed = 3
                OTHERS                 = 4.

            IF sy-subrc <> 0.
              RAISE EXCEPTION TYPE zcx_zabap_table_edit EXPORTING custom_message = |Insert multi CD error - { sy-subrc }|.
            ENDIF.
          ENDIF.
        ENDIF.

        "DELETED ENTRY
        IF deleted IS BOUND.
          ASSIGN deleted->* TO <deleted>.
          IF lines( <deleted> ) > 0.
            CALL FUNCTION 'CHANGEDOCUMENT_MULTIPLE_CASE2'
              EXPORTING
                change_indicator       = 'D'             " Change flag
                docu_delete            = abap_true
                tablename              = CONV tabname( table_name )                  " Name of the table structure of object class
                table_old              = <deleted>
                table_new              = <empty>
                docu_delete_if         = abap_true
              EXCEPTIONS
                nametab_error          = 1
                open_missing           = 2
                position_insert_failed = 3
                OTHERS                 = 4.

            IF sy-subrc <> 0.
              RAISE EXCEPTION TYPE zcx_zabap_table_edit EXPORTING custom_message = |Delete multi CD error - { sy-subrc }|.
            ENDIF.
          ENDIF.
        ENDIF.

        "MODIFIED ENTRY
        IF modified IS BOUND AND before_modified IS BOUND.
          ASSIGN modified->* TO <modified>.
          ASSIGN before_modified->* TO <before_modified>.
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
              RAISE EXCEPTION TYPE zcx_zabap_table_edit EXPORTING custom_message = |Change multi CD error - { sy-subrc }|.
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

  METHOD change_single.
    TRY.
        set_force_cd( force_marker = force_cd_on_all_fields table_name = table_name  ).

        FIELD-SYMBOLS:
          <inserted>        TYPE any,
          <deleted>         TYPE any,
          <before_modified> TYPE any,
          <modified>        TYPE any.

        "NEW ENTRY
        IF inserted IS BOUND.
          ASSIGN inserted->* TO <inserted>.

          CALL FUNCTION 'CHANGEDOCUMENT_SINGLE_CASE'
            EXPORTING
              change_indicator       = 'I'             " Change flag
              docu_insert            = 'X'
              tablename              = CONV tabname( table_name )                  " Name of the table structure of object class
              workarea_new           = <inserted>
            EXCEPTIONS
              nametab_error          = 1                " Error when calling NAMETAB_GET
              open_missing           = 2                " No OPEN was performed
              position_insert_failed = 3                " SQL error occurred during insert item
              OTHERS                 = 4.

          IF sy-subrc <> 0.
            RAISE EXCEPTION TYPE zcx_zabap_table_edit EXPORTING custom_message = |Insert single CD error - { sy-subrc }|.
          ENDIF.

        ENDIF.

        "DELETED ENTRY
        IF deleted IS BOUND.
          ASSIGN deleted->* TO <deleted>.

          CALL FUNCTION 'CHANGEDOCUMENT_SINGLE_CASE'
            EXPORTING
              change_indicator       = 'D'             " Change flag
              docu_delete            = 'X'
              tablename              = CONV tabname( table_name )                 " Name of the table structure of object class
              workarea_old           = <deleted>
            EXCEPTIONS
              nametab_error          = 1                " Error when calling NAMETAB_GET
              open_missing           = 2                " No OPEN was performed
              position_insert_failed = 3                " SQL error occurred during insert item
              OTHERS                 = 4.

          IF sy-subrc <> 0.
            RAISE EXCEPTION TYPE zcx_zabap_table_edit EXPORTING custom_message = |Delete single CD error - { sy-subrc }|.
          ENDIF.

        ENDIF.

        "MODIFIED ENTRY
        IF modified IS BOUND AND before_modified IS BOUND.
          ASSIGN modified->* TO <modified>.
          ASSIGN before_modified->* TO <before_modified>.

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
            RAISE EXCEPTION TYPE zcx_zabap_table_edit EXPORTING custom_message = |Change single CD error - { sy-subrc }|.
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
    table_fields->get_keys_structure( EXPORTING include_index_field = abap_true IMPORTING index_field_name = DATA(available_field_name) struct = data(key_struct) ).
    table_fields->get_table_with_add_fields(
        EXPORTING additional_fields = VALUE #( ( name = available_field_name type = CAST #( cl_abap_structdescr=>describe_by_name( 'CDCHNGINDH' ) ) ) )
        IMPORTING struct = DATA(struct) table = DATA(table) ).

    CREATE DATA table_with_indicator TYPE HANDLE table.

    FIELD-SYMBOLS:
      <table_with_indicator> TYPE table,
      <original_table>       TYPE table.
    ASSIGN table_with_indicator->* TO <table_with_indicator>.
    ASSIGN original_table->* TO <original_table>.

    <table_with_indicator> = CORRESPONDING #( <original_table> ).

    LOOP AT <table_with_indicator> ASSIGNING FIELD-SYMBOL(<row>).
      ASSIGN COMPONENT available_field_name OF STRUCTURE <row> TO FIELD-SYMBOL(<change_indicator>).
      <change_indicator> = indicator.
    ENDLOOP.

    IF sort = abap_true.
      "Create sort condition
      DATA sort_order TYPE abap_sortorder_tab .
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
    IF force_marker = abap_true.
      FIELD-SYMBOLS: <tabinfo> TYPE table.
      ASSIGN ('(SAPLSCD0)TABINFO') TO <tabinfo>.

      IF <tabinfo> IS ASSIGNED.
        DATA(where_clause) = |tabname = '{ table_name }' AND logflag = 'F'|.
        LOOP AT <tabinfo> ASSIGNING FIELD-SYMBOL(<tabinfo_row>) WHERE (where_clause).
          ASSIGN COMPONENT 'LOGFLAG' OF STRUCTURE <tabinfo_row> TO FIELD-SYMBOL(<logflag>).
          CLEAR: <logflag>.
        ENDLOOP.
      ENDIF.

      EXPORT zabap_force_logging = abap_false zabap_table_name = table_name TO MEMORY ID c_change_doc_force_log_all_fie.
    ENDIF.
  ENDMETHOD.

  METHOD set_force_cd.
    IF force_marker = abap_true.
      FIELD-SYMBOLS: <tabinfo> TYPE table.
      ASSIGN ('(SAPLSCD0)TABINFO') TO <tabinfo>.

      IF <tabinfo> IS ASSIGNED.
        DATA(where_clause) = |tabname = '{ table_name }' AND logflag = space|.
        LOOP AT <tabinfo> ASSIGNING FIELD-SYMBOL(<tabinfo_row>) WHERE (where_clause).
          ASSIGN COMPONENT 'LOGFLAG' OF STRUCTURE <tabinfo_row> TO FIELD-SYMBOL(<logflag>).
          <logflag> = 'F'.
        ENDLOOP.
      ENDIF.

      EXPORT zabap_force_logging = abap_true zabap_table_name = table_name TO MEMORY ID c_change_doc_force_log_all_fie.
    ENDIF.
  ENDMETHOD.

ENDCLASS.
