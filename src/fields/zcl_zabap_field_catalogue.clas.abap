CLASS zcl_zabap_field_catalogue DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    TYPES:
        tt_field_cat TYPE STANDARD TABLE OF lvc_s_fcat WITH DEFAULT KEY
        WITH NON-UNIQUE SORTED KEY name COMPONENTS fieldname
        WITH NON-UNIQUE SORTED KEY key_col COMPONENTS key
        WITH NON-UNIQUE SORTED KEY col_pos COMPONENTS col_pos.

    CLASS-METHODS:
      "! @raising CX_SY_MOVE_CAST_ERROR | <p class="shorttext synchronized">Raised if RTTS doesn't return cl_abap_structdescr</p>
      get_fc_from_struct_name IMPORTING name TYPE string RETURNING VALUE(field_catalogue) TYPE tt_field_cat RAISING CX_SY_MOVE_CAST_ERROR,
      get_fc_from_structdescr IMPORTING structdescr TYPE REF TO cl_abap_structdescr RETURNING VALUE(field_catalogue) TYPE tt_field_cat,
      "! @raising CX_SY_MOVE_CAST_ERROR | <p class="shorttext synchronized">Raised if RTTS doesn't return cl_abap_structdescr</p>
      get_fc_from_struct IMPORTING struct TYPE any RETURNING VALUE(field_catalogue) TYPE tt_field_cat RAISING CX_SY_MOVE_CAST_ERROR.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.


CLASS zcl_zabap_field_catalogue IMPLEMENTATION.
  METHOD get_fc_from_struct_name.
    DATA(typedescr) = cl_abap_structdescr=>describe_by_name( name ).
    field_catalogue = get_fc_from_structdescr( CAST cl_abap_structdescr( typedescr ) ).
  ENDMETHOD.
  METHOD get_fc_from_struct.
    DATA(typedescr) = cl_abap_structdescr=>describe_by_data( struct ).
    field_catalogue = get_fc_from_structdescr( CAST cl_abap_structdescr( typedescr ) ).
  ENDMETHOD.

  METHOD get_fc_from_structdescr.
    DATA(index) = 1.
    LOOP AT structdescr->get_ddic_field_list( ) REFERENCE INTO DATA(ddic_field).
      DATA(field) = CORRESPONDING lvc_s_fcat( ddic_field->* MAPPING key = keyflag ref_field = lfieldname
                                                                    ref_table = tabname dd_roll = rollname ).
      field-tabname  = 1.
      field-col_pos = index.

      IF field-domname = 'XFELD' OR field-domname = 'XFIELD'.
        field-checkbox = abap_true.
      ENDIF.

      IF field-domname IS INITIAL.
        field-coltext = ddic_field->fieldtext.
      ENDIF.

      APPEND field TO field_catalogue.

      index = index + 1.
    ENDLOOP.
  ENDMETHOD.
ENDCLASS.
