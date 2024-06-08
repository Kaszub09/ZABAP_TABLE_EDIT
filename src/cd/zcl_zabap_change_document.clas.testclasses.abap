CLASS tcl_non_db_methods DEFINITION DEFERRED.
CLASS zcl_zabap_change_document DEFINITION LOCAL FRIENDS tcl_non_db_methods.
CLASS tcl_non_db_methods DEFINITION FINAL FOR TESTING RISK LEVEL HARMLESS DURATION SHORT.

  PRIVATE SECTION.
    "Copied from LSCD0TOP
    TYPES:
      BEGIN OF ty_tabinfo,
        tabname   TYPE dfies-tabname,                       "B20K8A0G4I
        fieldname TYPE dfies-fieldname,                     "B20K8A0G4I
        rollname  TYPE dfies-rollname,                      "B20K8A0MNS
        keyflag   TYPE dfies-keyflag,                       "B20K8A0G4I
        reftable  TYPE dfies-reftable,                      "B20K8A0G4I
        reffield  TYPE dfies-reffield,                      "B20K8A0G4I
        logflag   TYPE dfies-logflag,                       "B20K8A0G4I
        datatype  TYPE dfies-datatype,                      "B20K8A0G4I
        inttype   TYPE dfies-inttype,                       "B20K8A0G4I
        outputlen TYPE i,
      END  OF ty_tabinfo,
      tt_in_memory_cd_tab TYPE STANDARD TABLE OF ty_tabinfo WITH EMPTY KEY.

    METHODS:
      setup,
      empty_cd_throw_error           FOR TESTING,
      empty_cd_dont_throw_error      FOR TESTING,
      create_table_with_indicator    FOR TESTING,
      create_sorted_table_with_indic FOR TESTING,
      create_empty_table             FOR TESTING,
      clear_force_cd                 FOR TESTING,
      set_force_cd                   FOR TESTING,
      call_force_cd IMPORTING in_memory_tab TYPE tt_in_memory_cd_tab expected TYPE tt_in_memory_cd_tab force_marker TYPE abap_bool
                 set_force TYPE abap_bool DEFAULT abap_false clear_force TYPE abap_bool DEFAULT abap_false.

    CONSTANTS:
      c_table_name  TYPE string VALUE 'ZABAP_TE_CD_TEST',
      c_objectclass TYPE cdobjectcl VALUE 'OBJECTCLASS',
      c_objectid    TYPE cdobjectv  VALUE 'CDOBJECTV'.

    DATA:
      cut TYPE REF TO zcl_zabap_change_document.
ENDCLASS.

CLASS tcl_non_db_methods IMPLEMENTATION.
  METHOD setup.
    cut = NEW #( objectclass = c_objectclass objectid = c_objectid ).
  ENDMETHOD.

  METHOD empty_cd_throw_error.
    cut->zif_zabap_change_document~open( ).
    TRY.
        cut->zif_zabap_change_document~close( ).
        cl_abap_unit_assert=>fail( 'Exception not thrown' ).
      CATCH zcx_zabap_table_edit.
    ENDTRY.
  ENDMETHOD.

  METHOD empty_cd_dont_throw_error.
    cut->zif_zabap_change_document~open( ).
    cut->zif_zabap_change_document~close( skip_exception_if_no_changes = abap_true ).
  ENDMETHOD.

  METHOD create_table_with_indicator.
    TYPES:
      tt_zabap_te_cd_test TYPE STANDARD TABLE OF zabap_te_cd_test WITH EMPTY KEY,
      BEGIN OF t_zabap_te_cd_test_ext.
        INCLUDE TYPE zabap_te_cd_test.
    TYPES:
      indicator TYPE c LENGTH 1,
    END OF t_zabap_te_cd_test_ext,
    tt_zabap_te_cd_test_ext TYPE STANDARD TABLE OF t_zabap_te_cd_test_ext WITH EMPTY KEY.

    DATA(original_table) = VALUE tt_zabap_te_cd_test( ( key_track = '2' ) ( key_track = '1' ) ).

    DATA(table_with_indicator) = cut->zif_zabap_change_document~create_table_with_indicator( table_name = 'ZABAP_TE_CD_TEST'
      original_table = REF #( original_table ) indicator = 'U' sort = abap_false ).
    assign_to_table_fs table_with_indicator->* <table_with_indicator>.

    cl_abap_unit_assert=>assert_equals( exp = VALUE tt_zabap_te_cd_test_ext( indicator = 'U' ( key_track = '2' ) ( key_track = '1' ) )
                      act = <table_with_indicator> ).
  ENDMETHOD.

  METHOD create_sorted_table_with_indic.
    TYPES: tt_zabap_te_cd_test TYPE STANDARD TABLE OF zabap_te_cd_test WITH EMPTY KEY.

    DATA(original_table) = VALUE tt_zabap_te_cd_test( ( dec_with_sign = '1' ) ( dec_with_sign = '-1' ) ( dec_with_sign = '-10' ) ).

    DATA(table_with_indicator) = cut->zif_zabap_change_document~create_table_with_indicator( table_name = 'ZABAP_TE_CD_TEST'
      original_table = REF #( original_table ) sort = abap_true ).
    assign_to_table_fs table_with_indicator->* <table_with_indicator>.

    ASSIGN COMPONENT 'DEC_WITH_SIGN' OF STRUCTURE <table_with_indicator>[ 1 ] TO FIELD-SYMBOL(<dec_with_sign>).
    cl_abap_unit_assert=>assert_equals( exp = '-10' act = <dec_with_sign> ).
    ASSIGN COMPONENT 'DEC_WITH_SIGN' OF STRUCTURE <table_with_indicator>[ 2 ] TO <dec_with_sign>.
    cl_abap_unit_assert=>assert_equals( exp = '-1' act = <dec_with_sign> ).
    ASSIGN COMPONENT 'DEC_WITH_SIGN' OF STRUCTURE <table_with_indicator>[ 3 ] TO <dec_with_sign>.
    cl_abap_unit_assert=>assert_equals( exp = '1' act = <dec_with_sign> ).
  ENDMETHOD.

  METHOD create_empty_table.
    DATA(empty_table) = cut->create_empty_table( 'ZABAP_TE_CD_TEST' ).
    assign_to_table_fs empty_table->* <empty_table>.
    cl_abap_unit_assert=>assert_equals( exp = 0 act = lines( <empty_table> ) ).
  ENDMETHOD.

  METHOD clear_force_cd.
    DATA(in_memory_tab) = VALUE tt_in_memory_cd_tab( tabname = c_table_name
      ( keyflag = 'X' logflag = 'X' ) ( keyflag = 'X' logflag = 'F' ) ( keyflag = 'X' logflag = ' ' )
      ( logflag = 'X' ) ( logflag = 'F' ) ( logflag = ' ' ) ).
    DATA(expected) = VALUE tt_in_memory_cd_tab( tabname = c_table_name
      ( keyflag = 'X' logflag = 'X' ) ( keyflag = 'X' logflag = ' ' ) ( keyflag = 'X' logflag = ' ' )
      ( logflag = 'X' ) ( logflag = ' ' ) ( logflag = ' ' ) ).

    call_force_cd( in_memory_tab = in_memory_tab expected = expected force_marker = abap_true clear_force = abap_true ).
    call_force_cd( in_memory_tab = in_memory_tab expected = in_memory_tab force_marker = abap_false clear_force = abap_true ).
  ENDMETHOD.

  METHOD set_force_cd.
    DATA(in_memory_tab) = VALUE tt_in_memory_cd_tab( tabname = c_table_name
      ( keyflag = 'X' logflag = 'X' ) ( keyflag = 'X' logflag = 'F' ) ( keyflag = 'X' logflag = ' ' )
      ( logflag = 'X' ) ( logflag = 'F' ) ( logflag = ' ' ) ).
    DATA(expected) = VALUE tt_in_memory_cd_tab( tabname = c_table_name
      ( keyflag = 'X' logflag = 'X' ) ( keyflag = 'X' logflag = 'F' ) ( keyflag = 'X' logflag = 'F' )
      ( logflag = 'X' ) ( logflag = 'F' ) ( logflag = 'F' ) ).

    call_force_cd( in_memory_tab = in_memory_tab expected = expected force_marker = abap_true set_force = abap_true ).
    call_force_cd( in_memory_tab = in_memory_tab expected = in_memory_tab force_marker = abap_false set_force = abap_true ).
  ENDMETHOD.

  METHOD call_force_cd.
    "Needed to initialize program and table in memory
    cut->zif_zabap_change_document~open( ).
    cut->zif_zabap_change_document~close( skip_exception_if_no_changes = abap_true ).
    assign_to_table_fs ('(SAPLSCD0)TABINFO') <tabinfo>.
    <tabinfo> = in_memory_tab.

    IF set_force = abap_true.
      cut->set_force_cd( force_marker = force_marker table_name = c_table_name ).
    ENDIF.
    IF clear_force = abap_true.
      cut->clear_force_cd( force_marker = force_marker table_name = c_table_name ).
    ENDIF.

    cl_abap_unit_assert=>assert_equals( exp = expected act = <tabinfo> ).
  ENDMETHOD.
ENDCLASS.
"=================================================================
" You can change rollback to commit in order to create and check change document manually
"=================================================================
CLASS tcl_db_methods DEFINITION DEFERRED.
CLASS zcl_zabap_change_document DEFINITION LOCAL FRIENDS tcl_db_methods.
CLASS tcl_db_methods DEFINITION FINAL FOR TESTING RISK LEVEL HARMLESS DURATION SHORT.

  PRIVATE SECTION.
    TYPES:
        tt_zabap_te_cd_test TYPE STANDARD TABLE OF zabap_te_cd_test WITH EMPTY KEY.

    CLASS-METHODS:
      class_setup,
      class_teardown.

    METHODS:
      teardown,
      save_tracked_fields     FOR TESTING,
      save_non_tracked_fields FOR TESTING,
      save_key_only           FOR TESTING,
      save_non_initial        FOR TESTING,
      save_all_fields         FOR TESTING,
      save_multi              FOR TESTING.

    CONSTANTS:
      c_objectclass TYPE cdobjectcl VALUE 'OBJECTCLASS'.

    CLASS-DATA:
      table_name             TYPE string VALUE 'ZABAP_TE_CD_TEST',
      single_deleted         TYPE zabap_te_cd_test,
      single_inserted        TYPE zabap_te_cd_test,
      single_before_modified TYPE zabap_te_cd_test,
      single_modified        TYPE zabap_te_cd_test,
      multi_deleted          TYPE tt_zabap_te_cd_test,
      multi_inserted         TYPE tt_zabap_te_cd_test,
      multi_before_modified  TYPE tt_zabap_te_cd_test,
      multi_modified         TYPE tt_zabap_te_cd_test.

    DATA:
     cut          TYPE REF TO zif_zabap_change_document.
ENDCLASS.

CLASS tcl_db_methods IMPLEMENTATION.
  METHOD class_teardown.
    ROLLBACK WORK.
    "COMMIT WORK.
  ENDMETHOD.

  METHOD teardown.
    ROLLBACK WORK.
    "COMMIT WORK.
  ENDMETHOD.

  METHOD class_setup.
    single_deleted = VALUE zabap_te_cd_test( key_no_track = 'DKNT' non_key_no_track = 'DNE' ).
    single_inserted = VALUE zabap_te_cd_test( key_no_track = 'IKNT' non_key_no_track = 'INE' ).
    single_before_modified = VALUE zabap_te_cd_test( key_track = 'BMKT' non_key_no_track = 'OLD' non_key_track = 'OLD' ).
    single_modified = VALUE zabap_te_cd_test( key_track = 'MKT' non_key_no_track = 'NEW' non_key_track = 'NEW' ).

    multi_before_modified = VALUE tt_zabap_te_cd_test( non_key_track = '1' ( dec_with_sign = '12' ) ( dec_with_sign = '-12' ) ( dec_with_sign = '-40' ) ).
    multi_modified = VALUE tt_zabap_te_cd_test( non_key_track = '2' ( dec_with_sign = '-40' ) ( dec_with_sign = '12' ) ( dec_with_sign = '-12' ) ).
    multi_deleted = VALUE tt_zabap_te_cd_test( ( key_track = 'DEL_1' ) ).
    multi_inserted = VALUE tt_zabap_te_cd_test( ( key_track = 'INS_1' ) ).
  ENDMETHOD.

  METHOD save_non_tracked_fields.
    cut = NEW zcl_zabap_change_document( objectclass = c_objectclass objectid = 'SAVE_NON_TRACKED_FIELDS' ).
    cut->open( ).
    cut->change_single( table_name = table_name force_cd_on_all_fields = abap_true
      before_modified = REF #( single_before_modified ) modified = REF #( single_modified ) ).
    DATA(change_nr) = cut->close( ).

    "Check result
    SELECT * FROM cdpos
      WHERE changenr = @change_nr AND ( ( fname = 'NON_KEY_NO_TRACK' AND value_old = 'OLD' AND value_new = 'NEW' )
                       OR ( fname = 'NON_KEY_TRACK' AND value_old = 'OLD' AND value_new = 'NEW' ) )
      INTO TABLE @DATA(cdpos).

    cl_abap_unit_assert=>assert_equals( exp = 2 act = lines( cdpos ) ).
  ENDMETHOD.

  METHOD save_tracked_fields.
    cut = NEW zcl_zabap_change_document( objectclass = c_objectclass objectid = 'SAVE_TRACKED_FIELDS' ).
    cut->open( ).
    cut->change_single( table_name = table_name force_cd_on_all_fields = abap_false
      before_modified = REF #( single_before_modified ) modified = REF #( single_modified ) ).
    DATA(change_nr) = cut->close( ).

    "Check result
    SELECT * FROM cdpos
      WHERE changenr = @change_nr AND ( ( fname = 'NON_KEY_NO_TRACK' AND value_old = 'OLD' AND value_new = 'NEW' )
                       OR ( fname = 'NON_KEY_TRACK' AND value_old = 'OLD' AND value_new = 'NEW' ) )
      INTO TABLE @DATA(cdpos).

    cl_abap_unit_assert=>assert_equals( exp = 1 act = lines( cdpos ) ).
  ENDMETHOD.

  METHOD save_key_only.
    cut = NEW zcl_zabap_change_document( objectclass = c_objectclass objectid = 'SAVE_KEY_ONLY' ).
    cut->open( ).
    cut->change_single( table_name = table_name force_cd_on_all_fields = abap_true save_fields = cut->c_save_none
      inserted = REF #( single_inserted ) deleted = REF #( single_deleted ) ).
    DATA(change_nr) = cut->close( ).

    "Check result
    SELECT * FROM cdpos
      WHERE changenr = @change_nr AND ( tabkey LIKE '%DKNT%' OR tabkey LIKE '%IKNT%' ) AND fname = 'KEY'
      INTO TABLE @DATA(cdpos).

    cl_abap_unit_assert=>assert_equals( exp = 2 act = lines( cdpos ) ).
  ENDMETHOD.

  METHOD save_non_initial.
    cut = NEW zcl_zabap_change_document( objectclass = c_objectclass objectid = 'SAVE_NON_INITIAL' ).
    cut->open( ).
    cut->change_single( table_name = table_name force_cd_on_all_fields = abap_true save_fields = cut->c_save_non_initial
      inserted = REF #( single_inserted ) deleted = REF #( single_deleted ) ).
    DATA(change_nr) = cut->close( ).

    "Check result
    SELECT * FROM cdpos
      WHERE changenr = @change_nr AND ( tabkey LIKE '%DKNT%' OR tabkey LIKE '%IKNT%' ) AND fname = 'NON_KEY_NO_TRACK'
      INTO TABLE @DATA(cdpos).

    cl_abap_unit_assert=>assert_equals( exp = 2 act = lines( cdpos ) ).
  ENDMETHOD.

  METHOD save_all_fields.
    cut = NEW zcl_zabap_change_document( objectclass = c_objectclass objectid = 'SAVE_ALL_FIELDS' ).
    cut->open( ).
    cut->change_single( table_name = table_name force_cd_on_all_fields = abap_true save_fields = cut->c_save_all
      inserted = REF #( single_inserted ) deleted = REF #( single_deleted ) ).
    DATA(change_nr) = cut->close( ).

    "Check result
    SELECT * FROM cdpos
      WHERE changenr = @change_nr AND ( tabkey LIKE '%DKNT%' OR tabkey LIKE '%IKNT%' ) AND fname IN ( 'NON_KEY_TRACK', 'NON_KEY_NO_TRACK' )
      INTO TABLE @DATA(cdpos).

    cl_abap_unit_assert=>assert_equals( exp = 4 act = lines( cdpos ) ).
  ENDMETHOD.

  METHOD save_multi.
    cut = NEW zcl_zabap_change_document( objectclass = c_objectclass objectid = 'SAVE_MULTI' ).
    cut->open( ).
    cut->change_multi( table_name = table_name
      before_modified = cut->create_table_with_indicator( table_name = table_name original_table = REF #( multi_before_modified ) indicator = 'U' sort = abap_true )
      modified = cut->create_table_with_indicator( table_name = table_name original_table = REF #( multi_modified ) indicator = 'U' sort = abap_true )
      deleted = cut->create_table_with_indicator( table_name = table_name original_table = REF #( multi_deleted ) indicator = 'D' )
      inserted = cut->create_table_with_indicator( table_name = table_name original_table = REF #( multi_inserted ) indicator = 'I' ) ).
    DATA(change_nr) = cut->close( ).

    "Check result
    SELECT * FROM cdpos
      WHERE changenr = @change_nr AND tabkey <> @space
      INTO TABLE @DATA(cdpos).

    cl_abap_unit_assert=>assert_equals( exp = 5 act = lines( cdpos ) ).
  ENDMETHOD.
ENDCLASS.
