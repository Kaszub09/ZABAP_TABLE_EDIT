CLASS zcl_zabap_table_edit_restr_sel DEFINITION PUBLIC FINAL CREATE PRIVATE GLOBAL FRIENDS zcl_zabap_table_edit_factory.

  PUBLIC SECTION.
    INTERFACES:
      zif_zabap_table_edit_restr_sel.

    METHODS:
      constructor IMPORTING table_name TYPE string.

  PRIVATE SECTION.
    METHODS:
      init_selection RAISING zcx_zabap_table_edit,
      selection_dialog RETURNING VALUE(changed) TYPE abap_bool RAISING zcx_zabap_table_edit.

    DATA:
      selection_id TYPE dynselid,
      table_name   TYPE string,
      messages     TYPE REF TO zcl_zabap_table_edit_messages.

    DATA:
      BEGIN OF selection,
        where_clauses TYPE rsds_twhere,
        field_ranges  TYPE rsds_trange,
        fields_tab    TYPE STANDARD TABLE OF rsdsfields WITH EMPTY KEY,
      END OF selection.
ENDCLASS.

CLASS zcl_zabap_table_edit_restr_sel IMPLEMENTATION.
  METHOD constructor.
    me->table_name = table_name.
    messages = NEW #( ).
  ENDMETHOD.

  METHOD zif_zabap_table_edit_restr_sel~display.
    IF messages->confirm_data_loss_on_selection( will_data_be_lost_on_change ) = abap_false.
      RETURN.
    ENDIF.

    IF selection_id IS INITIAL.
      init_selection( ).
    ENDIF.

    changed = selection_dialog( ).
  ENDMETHOD.

  METHOD zif_zabap_table_edit_restr_sel~get_where_cond.
    where = VALUE #( selection-where_clauses[ tablename = table_name ]-where_tab OPTIONAL ).
  ENDMETHOD.

  METHOD init_selection.
    DATA tables_tab TYPE STANDARD TABLE OF rsdstabs.

    APPEND VALUE #( prim_tab = table_name ) TO tables_tab.

    DATA fields_tab TYPE STANDARD TABLE OF rsdsfields.
    " Don't use secondary keys because they don't preserve field order
    fields_tab = VALUE #( FOR field IN zcl_zabap_field_catalogue=>get_fc_from_struct_name( table_name )
        WHERE ( key = abap_true and fieldname <> 'MANDT' and datatype <> 'CLNT' ) ( tablename = table_name fieldname = field-fieldname ) ).

    CALL FUNCTION 'FREE_SELECTIONS_INIT'
      EXPORTING
        kind                     = 'T'
      IMPORTING
        selection_id             = selection_id
      TABLES
        tables_tab               = tables_tab
        fields_tab               = fields_tab
      EXCEPTIONS
        fields_incomplete        = 1                " Only PRIM_FNAME filled
        fields_no_join           = 2                " Field assignment without join
        field_not_found          = 3                " Dictionary field not found
        no_tables                = 4                " Table P_TABLES is empty
        table_not_found          = 5                " A table from P_TABLES was not found
        expression_not_supported = 6                " Expression not (yet) supported
        incorrect_expression     = 7                " Incorrect logical expression
        illegal_kind             = 8                " KIND not equal to T,G,F or: G without P_RSDSQCAT
        area_not_found           = 9                " Invalid key FIELD_GROUPS_KEY
        inconsistent_area        = 10               " Inconsistent selection view
        kind_f_no_fields_left    = 11               " KIND = F, but no field left after cleanup
        kind_f_no_fields         = 12               " KIND = F, but no field passed
        too_many_fields          = 13               " Too many entries in FIELDS_TAB
        dup_field                = 14               " Field doubles in FIELDS_TAB
        field_no_type            = 15               " No field type in the field description
        field_ill_type           = 16               " Non-allowed field type
        dup_event_field          = 17               " Field doubled in EVENT_FIELDS
        node_not_in_ldb          = 18               " Node not part of logical database
        area_no_field            = 19               " Selection view has no fields
        OTHERS                   = 20.

    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE zcx_zabap_table_edit EXPORTING custom_message = |FREE_SELECTIONS_INIT error { sy-subrc }|.
    ENDIF.
  ENDMETHOD.

  METHOD selection_dialog.
    changed = abap_false.

    DATA:
      where_clauses TYPE rsds_twhere,
      field_ranges  TYPE rsds_trange,
      fields_tab    TYPE STANDARD TABLE OF rsdsfields WITH EMPTY KEY.

    CALL FUNCTION 'FREE_SELECTIONS_DIALOG'
      EXPORTING
        selection_id    = selection_id
        as_window       = 'X'
      IMPORTING
        where_clauses   = where_clauses
        field_ranges    = field_ranges
      TABLES
        fields_tab      = fields_tab
      EXCEPTIONS
        internal_error  = 1                " Internal error
        no_action       = 2                " Canceled by user
        selid_not_found = 3                " Transfer non-existent selection ID
        illegal_status  = 4                " Invalid status number
        OTHERS          = 5.

    IF sy-subrc <> 0 AND sy-subrc <> 2.
      RAISE EXCEPTION TYPE zcx_zabap_table_edit EXPORTING custom_message = |FREE_SELECTIONS_DIALOG error { sy-subrc }|.
    ENDIF.

    IF where_clauses = selection-where_clauses OR sy-subrc = 2.
      "^Nothing to change
      RETURN.
    ENDIF.

    selection = VALUE #( where_clauses = where_clauses field_ranges = field_ranges fields_tab = fields_tab ).
    changed = abap_true.
  ENDMETHOD.

  METHOD zif_zabap_table_edit_restr_sel~get_field_ranges.
    field_ranges = VALUE #( selection-field_ranges[ tablename = table_name ]-frange_t OPTIONAL ).
  ENDMETHOD.
ENDCLASS.
