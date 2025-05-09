CLASS zcl_zabap_table_fields DEFINITION PUBLIC CREATE PUBLIC.

  PUBLIC SECTION.
    METHODS:
        constructor IMPORTING table_name TYPE string editable TYPE abap_bool DEFAULT abap_false,
        get_field_catalogue RETURNING VALUE(field_catalogue) TYPE zcl_zabap_field_catalogue=>tt_field_cat,
        "! <p class="shorttext synchronized">Based on key fields of table specified in constructor</p>
        "! @parameter include_index_field | <p class="shorttext synchronized">Appends integer field at the end of struct</p>
        "! @parameter struct | <p class="shorttext synchronized">Key fields + index field if applicable</p>
        "! @parameter table | <p class="shorttext synchronized">Key fields + index field if applicable</p>
        "! @parameter index_field_name | <p class="shorttext synchronized">Name of appended integer field if applicable</p>
        get_keys_structure IMPORTING include_index_field TYPE abap_bool
                           EXPORTING struct TYPE REF TO cl_abap_structdescr table TYPE REF TO cl_abap_tabledescr
                                     index_field_name TYPE string,
        get_base_with_add_fields IMPORTING additional_fields TYPE cl_abap_structdescr=>component_table
                                  EXPORTING struct TYPE REF TO cl_abap_structdescr table TYPE REF TO cl_abap_tabledescr,
        get_fc_with_add_fields IMPORTING additional_fields TYPE cl_abap_structdescr=>component_table
                               RETURNING VALUE(field_catalogue) TYPE zcl_zabap_field_catalogue=>tt_field_cat,
        get_non_keys_structure EXPORTING struct TYPE REF TO cl_abap_structdescr table TYPE REF TO cl_abap_tabledescr.

    DATA :
      has_mandant          TYPE abap_bool READ-ONLY,
      mandant_field        TYPE string READ-ONLY,
      key_fields_only      TYPE abap_bool READ-ONLY,
      is_in_edit_mode      TYPE abap_bool,
      is_in_technical_view TYPE abap_bool.

  PRIVATE SECTION.
    METHODS:
      get_available_index_name RETURNING VALUE(name) TYPE string,
      modify_fc CHANGING fc TYPE zcl_zabap_field_catalogue=>tt_field_cat.

    DATA:
      table_name      TYPE string,
      field_catalogue TYPE zcl_zabap_field_catalogue=>tt_field_cat.
ENDCLASS.

CLASS zcl_zabap_table_fields IMPLEMENTATION.
  METHOD constructor.
    me->table_name = table_name.
    field_catalogue = zcl_zabap_field_catalogue=>get_fc_from_struct_name( table_name ).

    LOOP AT field_catalogue REFERENCE INTO DATA(field).
      field->outputlen = field->outputlen + COND #( WHEN field->outputlen <= 10 THEN 3 ELSE 5 ).
    ENDLOOP.

    DATA(mandant) = REF #( field_catalogue[ datatype = 'CLNT'  ] OPTIONAL ).

    IF mandant IS BOUND.
      has_mandant = abap_true.
      mandant->no_out = abap_true.
      mandant_field = mandant->fieldname.

    ELSE.
      has_mandant = abap_false.

    ENDIF.

    key_fields_only = xsdbool( NOT line_exists( field_catalogue[ KEY key_col key = space ]  ) ).

    is_in_edit_mode = editable.
  ENDMETHOD.

  METHOD get_available_index_name.
    DATA(index) = 0.
    name = |INDEX_NOT_USED_{ index }|.
    WHILE line_exists( field_catalogue[ KEY name fieldname = name ] ).
      index = index + 1.
      name = |INDEX_NOT_USED_{ index }|.
    ENDWHILE.
  ENDMETHOD.

  METHOD get_fc_with_add_fields.
    field_catalogue = me->field_catalogue.
    DATA(index) = lines( field_catalogue ) + 1.

    LOOP AT additional_fields REFERENCE INTO DATA(additional_field).
      IF NOT additional_field->type IS INSTANCE OF cl_abap_elemdescr. CONTINUE. ENDIF.

      DATA(elemdescr) = CAST cl_abap_elemdescr( additional_field->type ).
      DATA(ddic_field) = elemdescr->get_ddic_field( ).

      DATA(field) = CORRESPONDING lvc_s_fcat( ddic_field MAPPING key = keyflag dd_roll = rollname ).
      field-fieldname = additional_field->name.
      field-tabname   = 1.
      field-col_pos   = index.
      APPEND field TO field_catalogue.

      index = index + 1.
    ENDLOOP.

    modify_fc( CHANGING fc = field_catalogue ).
  ENDMETHOD.

  METHOD get_field_catalogue.
    field_catalogue = me->field_catalogue.
    modify_fc( CHANGING fc = field_catalogue ).
  ENDMETHOD.

  METHOD get_keys_structure.
    DATA(components) = VALUE cl_abap_structdescr=>component_table( ).
    " Don't use secondary keys because they don't preserve field order
    LOOP AT field_catalogue REFERENCE INTO DATA(field) WHERE key = abap_true.
      DATA(name) = COND #( WHEN field->dd_roll IS INITIAL THEN |{ table_name }-{ field->fieldname }| ELSE |{ field->dd_roll }| ).
      APPEND VALUE #( name = field->fieldname type = CAST #( cl_abap_structdescr=>describe_by_name( name ) ) ) TO components.
    ENDLOOP.

    IF include_index_field = abap_true.
      index_field_name = get_available_index_name( ).
      APPEND VALUE #( name = index_field_name type = CAST #( cl_abap_structdescr=>describe_by_data( 0 ) ) ) TO components.
    ENDIF.

    struct = cl_abap_structdescr=>get( p_components = components ).
    table = cl_abap_tabledescr=>get( p_line_type = struct p_key_kind = cl_abap_tabledescr=>keydefkind_default ).
  ENDMETHOD.

  METHOD get_base_with_add_fields.
    DATA(keys) = VALUE abap_keydescr_tab( ).
    DATA(components) = VALUE cl_abap_structdescr=>component_table( ).

    LOOP AT field_catalogue REFERENCE INTO DATA(field).
      DATA(name) = COND #( WHEN field->dd_roll IS INITIAL THEN |{ table_name }-{ field->fieldname }| ELSE |{ field->dd_roll }| ).
      APPEND VALUE #( name = field->fieldname type = CAST #( cl_abap_structdescr=>describe_by_name( name ) ) ) TO components.
      IF field->key = abap_true.
        APPEND VALUE #( name  = field->fieldname ) TO keys.
      ENDIF.
    ENDLOOP.
    APPEND LINES OF additional_fields TO components.

    struct = cl_abap_structdescr=>get( p_components = components ).
    table = cl_abap_tabledescr=>get( p_line_type = struct p_key_kind = cl_abap_tabledescr=>keydefkind_user p_key = keys ).
  ENDMETHOD.

  METHOD get_non_keys_structure.
    DATA(components) = VALUE cl_abap_structdescr=>component_table( ).
    " Don't use secondary keys because they don't preserve field order
    LOOP AT field_catalogue REFERENCE INTO DATA(field) WHERE key = abap_false.
      DATA(name) = COND #( WHEN field->dd_roll IS INITIAL THEN |{ table_name }-{ field->fieldname }| ELSE |{ field->dd_roll }| ).
      APPEND VALUE #( name = field->fieldname type = CAST #( cl_abap_structdescr=>describe_by_name( name ) ) ) TO components.
    ENDLOOP.

    struct = cl_abap_structdescr=>get( p_components = components ).
    table = cl_abap_tabledescr=>get( p_line_type = struct p_key_kind = cl_abap_tabledescr=>keydefkind_default ).
  ENDMETHOD.


  METHOD modify_fc.
    LOOP AT fc REFERENCE INTO DATA(field).
      field->edit = is_in_edit_mode.

      IF is_in_technical_view = abap_true.
        field->no_convext = abap_true.
        field->checkbox = abap_false.
        field->coltext = field->reptext = field->scrtext_s = field->scrtext_m = field->scrtext_l = field->fieldname.
        field->tech_form = 90. "check include <SLVC_CELL_DATA_GET> for effect - 90 means direct copy without any formatting
      ENDIF.
    ENDLOOP.
  ENDMETHOD.

ENDCLASS.
