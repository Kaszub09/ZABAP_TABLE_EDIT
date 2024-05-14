CLASS zcl_zabap_table_edit_text_tab DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    METHODS:
      constructor IMPORTING original_table TYPE string change_doc_type TYPE zabap_change_doc_type,
      append_additional_fields CHANGING additional_fields TYPE cl_abap_structdescr=>component_table,
      update_row CHANGING row TYPE any,
      save IMPORTING initial TYPE REF TO data extended TYPE REF TO data,
      map_table_to_text_table IMPORTING table TYPE REF TO data  RETURNING VALUE(text_table) TYPE REF TO data,
      map_row_to_text_key IMPORTING row TYPE any  RETURNING VALUE(text_row) TYPE REF TO data,
      map_row_to_text_non_key IMPORTING row TYPE any  RETURNING VALUE(text_row) TYPE REF TO data,
      set_edit_mode IMPORTING edit_mode TYPE abap_bool EXPORTING error_message TYPE string RETURNING VALUE(locked) TYPE abap_bool,
      get_key_string IMPORTING row TYPE any RETURNING VALUE(key_string) TYPE string,
      should_be_updated IMPORTING field TYPE forfield RETURNING VALUE(should_be_updated) TYPE abap_bool,
      data_changed IMPORTING er_data_changed TYPE REF TO cl_alv_changed_data_protocol   .

    DATA:
    text_table_exists TYPE abap_bool READ-ONLY.
  PROTECTED SECTION.
  PRIVATE SECTION.
    TYPES:
      BEGIN OF t_text_cache,
        k   TYPE string,
        val TYPE REF TO data,
      END OF t_text_cache,
      tt_text_cache TYPE HASHED TABLE OF t_text_cache WITH UNIQUE KEY k.

    METHODS:
          create_change_doc IMPORTING inserted TYPE REF TO data deleted TYPE REF TO data
                                 before_modified TYPE REF TO data modified TYPE REF TO data
                          RAISING zcx_zabap_table_edit.
    DATA:
      original_table    TYPE string,
      text_table        TYPE tabname,
      text_field        TYPE forfield,
      text_key_mapping  TYPE STANDARD TABLE OF dd05p,
      taxt_table_fields TYPE STANDARD TABLE OF dd03l,
      change_doc_type   TYPE zabap_change_doc_type.


    DATA:
      select_clause TYPE string,
      lang_field    TYPE forfield,
      where_clause  TYPE string,
      map_table     TYPE cl_abap_corresponding=>mapping_table.

    DATA:
      keys_struct     TYPE REF TO  cl_abap_structdescr,
      non_keys_struct TYPE REF TO  cl_abap_structdescr.

    DATA:text_cache   TYPE tt_text_cache,
         table_locker TYPE REF TO zcl_zabap_table_edit_lock.
ENDCLASS.



CLASS zcl_zabap_table_edit_text_tab IMPLEMENTATION.
  METHOD constructor.

    me->original_table = original_table.
    me->change_doc_type = change_doc_type.
    SELECT SINGLE tabname, fieldname FROM dd08l
    WHERE checktable = @original_table AND frkart = 'TEXT'
    INTO ( @text_table, @text_field ).

    table_locker = NEW #( CONV #( text_table ) ).


    text_table_exists = COND #( WHEN strlen( text_table ) = 0 THEN abap_false ELSE abap_true ).
    IF text_table_exists = abap_false. RETURN. ENDIF.


    SELECT * FROM dd05p WHERE tabname = @text_table AND fieldname = @text_field INTO CORRESPONDING FIELDS OF TABLE @text_key_mapping.
    SELECT * FROM dd03l WHERE tabname = @text_table INTO CORRESPONDING FIELDS OF TABLE @taxt_table_fields.

    "SELECT (doens't make sense wihtout additional ifleds anyway)
    LOOP AT taxt_table_fields REFERENCE INTO DATA(field) WHERE keyflag IS INITIAL.
      select_clause = |{ select_clause }, { field->tabname }~{ field->fieldname }|.
    ENDLOOP.
    select_clause = zcl_string=>right( val = select_clause len = strlen( select_clause ) - 2 ).

    "Take first lang field found
    LOOP AT taxt_table_fields REFERENCE INTO DATA(field_lang) WHERE keyflag = abap_true AND domname = 'SPRAS'.
      lang_field = field_lang->fieldname.
      EXIT.
    ENDLOOP.

    "WHERE
    where_clause = |{ lang_field } = @SY-LANGU|.
    LOOP AT text_key_mapping REFERENCE INTO DATA(key) WHERE NOT forkey = 'CLIENT'.
      where_clause = |{ where_clause } AND { key->fortable }~{ key->forkey } = @<row>-{ key->checkfield }|.
      APPEND VALUE #( kind = 1 dstname = key->forkey srcname = key->checkfield ) TO map_table.
    ENDLOOP.


    DATA(table_fields) = NEW zcl_zabap_table_fields( CONV #( text_table ) ).
    table_fields->get_keys_structure( EXPORTING include_index_field = abap_false IMPORTING struct = keys_struct ).
    table_fields->get_non_keys_structure(  IMPORTING struct = non_keys_struct ).
  ENDMETHOD.

  METHOD append_additional_fields.
    LOOP AT taxt_table_fields REFERENCE INTO DATA(field) WHERE keyflag IS INITIAL.
      APPEND VALUE #( name = field->fieldname type = CAST #( cl_abap_datadescr=>describe_by_name(
      |{ field->tabname }-{ field->fieldname }| ) )  ) TO additional_fields.
    ENDLOOP.
  ENDMETHOD.

  METHOD update_row.
    ASSIGN row TO FIELD-SYMBOL(<row>).

    DATA(key_strin) = get_key_string( row ).


    DATA(text_chache_ref) = REF #( text_cache[ k = key_strin ] OPTIONAL ).
    IF NOT text_chache_ref IS BOUND.
      INSERT VALUE #( k = key_strin ) INTO TABLE text_cache REFERENCE INTO text_chache_ref.

      SELECT SINGLE (select_clause)
FROM (text_table) WHERE (where_clause)
INTO CORRESPONDING FIELDS OF @<row>.

      text_chache_ref->val =  map_row_to_text_non_key( row = row ).


    ELSE.
      ASSIGN text_chache_ref->val->* TO FIELD-SYMBOL(<non_keys>).
      row = CORRESPONDING #( BASE ( row ) <non_keys> ).

    ENDIF.


  ENDMETHOD.

  METHOD save.
    DATA(initial_text) = map_table_to_text_table( initial ).
        FIELD-SYMBOLS <initial_text>         TYPE table.
    ASSIGN initial_text->* TO <initial_text>.
    loop at <initial_text> ASSIGNING FIELD-SYMBOL(<initial_row>).
        update_row( changing row = <initial_row> ).
    endloop.

    DATA(extended_text) = map_table_to_text_table( extended  ).


    DATA(comparator) = NEW zcl_zabap_table_comparator( CONV #( text_table ) ).
    comparator->compare_tables(
      EXPORTING
        initial_data    = initial_text
        modified_data   = extended_text
      IMPORTING
       inserted        = DATA(inserted)
        deleted         = DATA(deleted)
        before_modified = DATA(before_modified)
        modified        = DATA(modified)
    ).

    DATA(table_fields) = NEW zcl_zabap_table_fields( CONV #( text_table ) ).

    "Again with unnecessary "FieLDs-sYmBOls"
    FIELD-SYMBOLS <modified>        TYPE table.
    FIELD-SYMBOLS <inserted>        TYPE table.
    FIELD-SYMBOLS <deleted>         TYPE table.
    ASSIGN modified->* TO <modified>.
    ASSIGN inserted->* TO <inserted>.
    ASSIGN deleted->* TO <deleted>.
    "Actual db changes
    DATA(where) = ||.
    LOOP AT taxt_table_fields REFERENCE INTO DATA(tab_field) WHERE keyflag = abap_true AND fieldname <> 'CLIENT'.
      where = |{ where } AND { tab_field->fieldname } = @<row_to_delete>-{ tab_field->fieldname }|.
    ENDLOOP.
    where = zcl_string=>right(  val = where  len = strlen( where ) - 5     ).

    LOOP AT <deleted> ASSIGNING FIELD-SYMBOL(<row_to_delete>).
      DELETE FROM (text_table) WHERE (where).
    ENDLOOP.


    IF table_fields->key_fields_only = abap_true.
      "^Can't use modify if all fields are key fields. Also in this case it's impossible to have modified entries.
      INSERT (text_table) FROM TABLE @<inserted> ACCEPTING DUPLICATE KEYS.

    ELSE.
      MODIFY (text_table) FROM TABLE @<modified>.
      MODIFY (text_table) FROM TABLE @<inserted>.

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



  ENDMETHOD.

  METHOD map_table_to_text_table.
    CREATE DATA text_table TYPE TABLE OF (me->text_table).
    FIELD-SYMBOLS: <text_table> TYPE ANY TABLE.
    FIELD-SYMBOLS: <table> TYPE ANY TABLE.
    ASSIGN table->* TO <table>.
    ASSIGN text_table->* TO <text_table>.



    cl_abap_corresponding=>create( EXPORTING source = <table> destination = <text_table> mapping = map_table
       )->execute( EXPORTING source = <table> CHANGING destination = <text_table> ).

    LOOP AT <text_table> ASSIGNING FIELD-SYMBOL(<row>).
      ASSIGN COMPONENT lang_field OF STRUCTURE <row> TO FIELD-SYMBOL(<language>).
      <language> = sy-langu.
    ENDLOOP.
  ENDMETHOD.

  METHOD create_change_doc.
    DATA(cd) = NEW zcl_zabap_change_document( objectclass = CONV #( text_table ) objectid = CONV #( text_table ) ).

    cd->open( ).
    cd->change_multi( EXPORTING force_cd_on_all_fields = COND #( WHEN change_doc_type = 'F' THEN abap_true ELSE abap_false )
                                 table_name = CONV #( text_table )
                                 deleted = cd->create_table_with_indicator( table_name = CONV #( text_table ) original_table = deleted indicator = 'D' )
                                 inserted = cd->create_table_with_indicator( table_name = CONV #( text_table ) original_table = inserted indicator = 'I' )
                                 before_modified = cd->create_table_with_indicator( table_name = CONV #( text_table ) original_table = before_modified indicator = 'U' )
                                 modified = cd->create_table_with_indicator( table_name = CONV #( text_table ) original_table = modified  ) ).
    "Some SAP magic to get initial t-code
    DATA: original_tcode TYPE sytcode.
    CALL 'GET_PARAM_TCOD' ID 'PTCOD' FIELD original_tcode.
    cd->close( tcode = original_tcode ).
  ENDMETHOD.





  METHOD map_row_to_text_key.
    CREATE DATA text_row TYPE HANDLE keys_struct.
    ASSIGN text_row->* TO FIELD-SYMBOL(<text_row>).

    cl_abap_corresponding=>create( EXPORTING source = row destination = <text_row> mapping = map_table
       )->execute( EXPORTING source = row CHANGING destination = <text_row> ).


  ENDMETHOD.

  METHOD map_row_to_text_non_key.
    CREATE DATA text_row TYPE HANDLE non_keys_struct.
    ASSIGN text_row->* TO FIELD-SYMBOL(<text_row>).

    cl_abap_corresponding=>create( EXPORTING source = row destination = <text_row> mapping = VALUE #( )
    )->execute( EXPORTING source = row CHANGING destination = <text_row> ).


  ENDMETHOD.

  METHOD set_edit_mode.
    IF edit_mode = abap_true.
      locked = table_locker->lock_table( IMPORTING error_message  = error_message  ).
    ELSE.

      table_locker->unlock_table(  ).
    ENDIF.
  ENDMETHOD.

  METHOD get_key_string.

    LOOP AT me->text_key_mapping  REFERENCE INTO DATA(key_map).
      ASSIGN COMPONENT key_map->checkfield OF STRUCTURE row TO FIELD-SYMBOL(<component_value>).
      key_string = |{ key_string }{ <component_value> }|.
    ENDLOOP.

  ENDMETHOD.

  METHOD should_be_updated.
    should_be_updated = xsdbool( line_exists( text_key_mapping[ checkfield = field ] ) ).
  ENDMETHOD.

  METHOD data_changed.
  Types:
      begin of t_cells_to_mod,
          i type i,
          fieldname type lvc_fname,
          value type ref to data,
      end of t_cells_to_mod,
      tt_cells_to_mod type standard table of t_cells_to_mod with empty key.
  data(cells_to_mod) = value tt_cells_to_mod(  ).

    FIELD-SYMBOLS <table> TYPE table.
    ASSIGN er_data_changed->mp_mod_rows->* TO <table>.
    LOOP AT er_data_changed->mt_mod_cells REFERENCE INTO DATA(mod_cell) GROUP BY mod_cell->tabix REFERENCE INTO DATA(group).
      LOOP AT GROUP group REFERENCE INTO DATA(cell).
        IF should_be_updated( field = cell->fieldname ).
          update_row( CHANGING row = <table>[ group->* ] ).
          LOOP AT taxt_table_fields REFERENCE INTO DATA(field) where keyflag = abap_false.
            ASSIGN COMPONENT field->fieldname OF STRUCTURE <table>[ group->* ] TO FIELD-SYMBOL(<value>).
            append value #( i = group->* fieldname = field->fieldname value = ref #( <value> ) ) to cells_to_mod.
            "er_data_changed->modify_cell( i_tabix = group->* i_fieldname = field->fieldname i_value = <value> ).
          ENDLOOP.
          EXIT.
        ENDIF.
      ENDLOOP.
    ENDLOOP.

    loop at cells_to_mod reference into data(cell_to_mod).
          ASSIGN  cell_to_mod->value->* TO FIELD-SYMBOL(<vall>).
      er_data_changed->modify_cell( i_tabix = cell_to_mod->i i_fieldname = cell_to_mod->fieldname i_value = <vall> ).
    endloop.


  ENDMETHOD.

ENDCLASS.
