CLASS zcl_zabap_table_edit_text_tab DEFINITION PUBLIC CREATE PRIVATE
  GLOBAL FRIENDS zcl_zabap_table_edit_factory.

  PUBLIC SECTION.
    INTERFACES:
      zif_zabap_table_edit_text_tab.

    ALIASES:
     append_additional_fields FOR zif_zabap_table_edit_text_tab~append_additional_fields,
     update_text_elements FOR zif_zabap_table_edit_text_tab~update_text_elements,
     save FOR zif_zabap_table_edit_text_tab~save,
     lock_table FOR zif_zabap_table_edit_text_tab~lock_table,
     unlock_table FOR zif_zabap_table_edit_text_tab~unlock_table.

    METHODS:
      constructor IMPORTING configuration TYPE zif_zabap_table_edit_text_tab=>t_config.

  PRIVATE SECTION.
    TYPES:
      BEGIN OF t_text_cache,
        k   TYPE string,
        val TYPE REF TO data,
      END OF t_text_cache,
      tt_text_cache TYPE HASHED TABLE OF t_text_cache WITH UNIQUE KEY k,
      "--------------------------------------------------
      BEGIN OF t_table_fields,
        fieldname TYPE fieldname,
        keyflag   TYPE keyflag,
        rollname  TYPE rollname,
        domname   TYPE domname,
      END OF t_table_fields,
      tt_table_fields TYPE STANDARD TABLE OF t_table_fields WITH EMPTY KEY,
      "--------------------------------------------------
      BEGIN OF t_key_field_mapping,
        original TYPE fieldname,
        text     TYPE fieldname,
      END OF t_key_field_mapping,
      tt_key_field_mapping TYPE STANDARD TABLE OF t_key_field_mapping WITH EMPTY KEY,
      "--------------------------------------------------
      BEGIN OF t_text_table,
        name            TYPE string,
        fields          TYPE tt_table_fields,
        key_mapping     TYPE tt_key_field_mapping,
        mapping_table   TYPE cl_abap_corresponding=>mapping_table,
        locker          TYPE REF TO zcl_zabap_table_edit_lock,
        BEGIN OF field,
          lang    TYPE forfield,
          mandant TYPE forfield,
        END OF field,
        key_fields_only TYPE abap_bool,
      END OF t_text_table.

    METHODS:
      get_ttable_info_from_db,
      get_text_elements_cache IMPORTING extended TYPE REF TO data RETURNING VALUE(cache) TYPE tt_text_cache,
      map_original_key_to_string IMPORTING row TYPE any RETURNING VALUE(string_key) TYPE string,
      map_to_text_table IMPORTING table TYPE REF TO data  RETURNING VALUE(text_table) TYPE REF TO data,
      create_change_doc IMPORTING inserted TYPE REF TO data deleted TYPE REF TO data
                                  before_modified TYPE REF TO data modified TYPE REF TO data
                          RAISING zcx_zabap_table_edit.

    DATA:
      ttable_data TYPE t_text_table,
      config      TYPE zif_zabap_table_edit_text_tab=>t_config.
ENDCLASS.


CLASS zcl_zabap_table_edit_text_tab IMPLEMENTATION.
  METHOD constructor.
    config = configuration.

    get_ttable_info_from_db( ).

    ttable_data-locker = NEW #( ttable_data-name ).

    DATA(table_fields) = NEW zcl_zabap_table_fields( ttable_data-name ).
    ttable_data-field-mandant = table_fields->mandant_field.
    ttable_data-key_fields_only = table_fields->key_fields_only.

    LOOP AT ttable_data-fields REFERENCE INTO DATA(field_lang) WHERE keyflag = abap_true AND domname = 'SPRAS'.
      ttable_data-field-lang = field_lang->fieldname.
      EXIT.
    ENDLOOP.

    ttable_data-mapping_table = VALUE #( FOR key IN ttable_data-key_mapping ( kind = 1 srcname = key-original dstname = key-text ) ).
  ENDMETHOD.

  METHOD get_ttable_info_from_db.
    SELECT SINGLE tabname, fieldname FROM dd08l
      WHERE checktable = @config-table_name AND frkart = 'TEXT'
      INTO ( @ttable_data-name, @DATA(fieldname) ).

    SELECT * FROM dd03l
      WHERE tabname = @ttable_data-name
      INTO CORRESPONDING FIELDS OF TABLE @ttable_data-fields.

    SELECT checkfield AS original, forkey AS text
      FROM dd05p
      WHERE tabname = @ttable_data-name AND fieldname = @fieldname
      INTO CORRESPONDING FIELDS OF TABLE @ttable_data-key_mapping.
  ENDMETHOD.

  METHOD append_additional_fields.
    LOOP AT ttable_data-fields REFERENCE INTO DATA(field) WHERE keyflag IS INITIAL.
      APPEND VALUE #( name = field->fieldname type = CAST #( cl_abap_datadescr=>describe_by_name(
        |{ ttable_data-name }-{ field->fieldname }| ) ) ) TO additional_fields.
    ENDLOOP.
  ENDMETHOD.

  METHOD update_text_elements.
    "Check to avoid empty FAE in cache
    assign_to_table_fs extended->* <extended>.
    IF lines( <extended> ) = 0. RETURN. ENDIF.

    DATA(cache) = get_text_elements_cache( extended ).

    LOOP AT <extended> ASSIGNING FIELD-SYMBOL(<original_row>).
      DATA(cache_ref) = REF #( cache[ k = map_original_key_to_string( <original_row> ) ] OPTIONAL ).
      IF cache_ref IS BOUND.
        ASSIGN cache_ref->val->* TO FIELD-SYMBOL(<text_non_key>).
        <original_row> = CORRESPONDING #( BASE ( <original_row> ) <text_non_key> ).
      ENDIF.
    ENDLOOP.
  ENDMETHOD.

  METHOD get_text_elements_cache.
    assign_to_table_fs extended->* <extended>.
    "Build where clause
    DATA(where) = |{ ttable_data-field-lang } = @SY-LANGU|.
    LOOP AT ttable_data-key_mapping REFERENCE INTO DATA(key) WHERE NOT text = ttable_data-field-mandant.
      where = |{ where } AND { ttable_data-name }~{ key->text } = @<extended>-{ key->original }|. "<extended> must be the same as in extended assignment name
    ENDLOOP.

    "Create text table to store data
    DATA text_table TYPE REF TO data.
    CREATE DATA text_table TYPE TABLE OF (ttable_data-name).
    assign_to_table_fs text_table->* <text_table>.

    SELECT * FROM (ttable_data-name)
      FOR ALL ENTRIES IN @<extended>
      WHERE (where)
      INTO CORRESPONDING FIELDS OF TABLE @<text_table>.

    "Get structs needed for cache
    DATA(table_fields) = NEW zcl_zabap_table_fields( ttable_data-name ).
    table_fields->get_keys_structure( EXPORTING include_index_field = abap_false IMPORTING struct = DATA(key_struct) ).
    table_fields->get_non_keys_structure(  IMPORTING struct = DATA(non_key_struct) ).

    "Build cache
    create_and_assign_data text_key key_struct <text_key>.
    LOOP AT <text_table> ASSIGNING FIELD-SYMBOL(<text_row>).
      create_and_assign_data text_non_key non_key_struct <text_non_key>. "Must be recreated every time, since reference is stored

      <text_key> = CORRESPONDING #( <text_row> ).
      DATA(text_key_string) = ||.
      text_key_string = <text_key>.

      <text_non_key> = CORRESPONDING #( <text_row> ).
      INSERT VALUE #( k = text_key_string val = text_non_key ) INTO TABLE cache.
    ENDLOOP.
  ENDMETHOD.

  METHOD map_original_key_to_string.
    LOOP AT ttable_data-key_mapping REFERENCE INTO DATA(key_mapping).
      ASSIGN COMPONENT key_mapping->original OF STRUCTURE row TO FIELD-SYMBOL(<component_value>).
      string_key = |{ string_key }{ <component_value> }|.
    ENDLOOP.
  ENDMETHOD.

  METHOD save.
    DATA(initial_text) = map_to_text_table( initial ).
    DATA(extended_text) = map_to_text_table( extended ).
    assign_to_table_fs initial_text->* <initial_text>.
    assign_to_table_fs extended_text->* <extended_text>.

    DATA(comparator) = NEW zcl_zabap_table_comparator( ttable_data-name ).
    comparator->compare_tables( EXPORTING initial_data = initial_text modified_data = extended_text
        IMPORTING inserted = DATA(inserted) deleted = DATA(deleted)
                  before_modified = DATA(before_modified) modified = DATA(modified) ).
    assign_to_table_fs modified->* <modified>.
    assign_to_table_fs inserted->* <inserted>.
    assign_to_table_fs deleted->* <deleted>.

    "Build where clause for deletion - we need to delete all languages
    DATA(where) = ||.
    LOOP AT ttable_data-fields REFERENCE INTO DATA(key) WHERE fieldname = ttable_data-field-mandant.
      where = |{ where } AND { ttable_data-name }~{ key->fieldname } = @<extended>-{ key->fieldname }|.
    ENDLOOP.
    where = substring( val = where off =  5 len = strlen( where ) - 5 ). "remove initial AND

    "Actual db changes
    LOOP AT <deleted> ASSIGNING FIELD-SYMBOL(<row_to_delete>).
      DELETE FROM (ttable_data-name) WHERE (where).
    ENDLOOP.

    IF ttable_data-key_fields_only = abap_true.
      "^Can't use modify if all fields are key fields. Also in this case it's impossible to have modified entries.
      INSERT (ttable_data-name) FROM TABLE @<inserted> ACCEPTING DUPLICATE KEYS.

    ELSE.
      MODIFY (ttable_data-name) FROM TABLE @<modified>.
      MODIFY (ttable_data-name) FROM TABLE @<inserted>.

    ENDIF.

    "Change doc creation
    IF config-change_doc_type <> space.
      create_change_doc( inserted = inserted deleted = deleted before_modified = before_modified modified = modified ).
    ENDIF.
  ENDMETHOD.

  METHOD map_to_text_table.
    CREATE DATA text_table TYPE TABLE OF (ttable_data-name).
    assign_to_table_fs table->* <table>.
    assign_to_table_fs text_table->* <text_table>.

    cl_abap_corresponding=>create( source = <table> destination = <text_table> mapping = ttable_data-mapping_table
       )->execute( EXPORTING source = <table> CHANGING destination = <text_table> ).

    LOOP AT <text_table> ASSIGNING FIELD-SYMBOL(<row>).
      ASSIGN COMPONENT ttable_data-field-lang OF STRUCTURE <row> TO FIELD-SYMBOL(<language>).
      <language> = sy-langu.
    ENDLOOP.
  ENDMETHOD.

  METHOD create_change_doc.
    DATA(cd) = NEW zcl_zabap_change_document( objectclass = CONV #( config-table_name ) objectid = CONV #( ttable_data-name ) ).

    cd->open( ).
    cd->change_multi( force_cd_on_all_fields = COND #( WHEN config-change_doc_type = 'F' THEN abap_true ELSE abap_false )
                       table_name = ttable_data-name
                       deleted = cd->create_table_with_indicator( table_name = ttable_data-name original_table = deleted indicator = 'D' )
                       inserted = cd->create_table_with_indicator( table_name = ttable_data-name original_table = inserted indicator = 'I' )
                       before_modified = cd->create_table_with_indicator( table_name = ttable_data-name original_table = before_modified indicator = 'U' )
                       modified = cd->create_table_with_indicator( table_name = ttable_data-name original_table = modified  ) ).

    "Some SAP magic to get initial t-code
    DATA: original_tcode TYPE sytcode.
    CALL 'GET_PARAM_TCOD' ID 'PTCOD' FIELD original_tcode.
    cd->close( tcode = original_tcode ).
  ENDMETHOD.

  METHOD zif_zabap_table_edit_text_tab~lock_table.
    locked = ttable_data-locker->lock_table( IMPORTING error_message  = error_message  ).
  ENDMETHOD.

  METHOD zif_zabap_table_edit_text_tab~unlock_table.
    ttable_data-locker->unlock_table( ).
  ENDMETHOD.
ENDCLASS.
