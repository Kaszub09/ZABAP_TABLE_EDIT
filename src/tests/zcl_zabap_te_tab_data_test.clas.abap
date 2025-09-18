CLASS zcl_zabap_te_tab_data_test DEFINITION PUBLIC CREATE PUBLIC.

  PUBLIC SECTION.
    INTERFACES:
      zif_zabap_table_edit_tab_data.

    METHODS:
      constructor.

  PRIVATE SECTION.
    METHODS:
      on_data_changed FOR EVENT data_changed OF cl_gui_alv_grid IMPORTING er_data_changed e_onf4 e_onf4_before e_onf4_after e_ucomm sender,
      yes_no IMPORTING title TYPE string DEFAULT space question TYPE string start_col TYPE i DEFAULT 25 start_row TYPE i DEFAULT 6
                               RETURNING VALUE(confirmed) TYPE abap_bool.

    DATA:
      table_name TYPE string VALUE 'ZABAP_TE_TEST',
      table_data TYPE STANDARD TABLE OF zabap_te_test WITH EMPTY KEY,
      grid       TYPE REF TO cl_gui_alv_grid.
ENDCLASS.

CLASS zcl_zabap_te_tab_data_test IMPLEMENTATION.
  METHOD constructor.
    zif_zabap_table_edit_tab_data~mandant_field = 'MANDT'.

    grid = NEW cl_gui_alv_grid( i_parent = zcl_zabap_screen_with_containe=>get_container( ) ).
    grid->register_edit_event( cl_gui_alv_grid=>mc_evt_modified ). "Allows to catch edit events
    grid->register_edit_event( cl_gui_alv_grid=>mc_evt_enter ). "Allows also to catch Enter
    SET HANDLER on_data_changed FOR grid.
  ENDMETHOD.

  METHOD zif_zabap_table_edit_tab_data~get_selected_row_key.
    grid->get_selected_rows( IMPORTING et_index_rows = DATA(selected_rows) ).
    IF lines( selected_rows ) = 1.
      MESSAGE |Selected { selected_rows[ 1 ]-index } | TYPE 'I'.
    ELSE.
      MESSAGE |No single row selected| TYPE 'I'.
    ENDIF.
  ENDMETHOD.

  METHOD zif_zabap_table_edit_tab_data~lock_table.
    MESSAGE |Table lock called| TYPE 'I'.
    locked = yes_no( 'Was lock successful?' ).
  ENDMETHOD.

  METHOD zif_zabap_table_edit_tab_data~reset_grid.
    MESSAGE |Grid reset called| TYPE 'I'.
    SELECT * FROM zabap_te_test INTO CORRESPONDING FIELDS OF TABLE @table_data.

    zif_zabap_table_edit_tab_data~was_data_changed = abap_false.
    DATA(fields) = NEW zcl_zabap_table_fields( editable = in_edit_mode table_name = table_name ).
    DATA(field_cat) = CORRESPONDING lvc_t_fcat( fields->get_field_catalogue( ) ).

    grid->set_table_for_first_display( CHANGING it_outtab = table_data it_fieldcatalog = field_cat ).
  ENDMETHOD.

  METHOD zif_zabap_table_edit_tab_data~save_data.
    MESSAGE |Save data called| TYPE 'I'.
  ENDMETHOD.

  METHOD zif_zabap_table_edit_tab_data~unlock_table.
    MESSAGE |Table unlock called| TYPE 'I'.
  ENDMETHOD.

  METHOD zif_zabap_table_edit_tab_data~validate.
    MESSAGE |Validation save called| TYPE 'I'.
    result = COND i( WHEN yes_no( 'Was validation successful?' ) = abap_true THEN zif_zabap_table_edit_data=>c_validation-ok
                     ELSE zif_zabap_table_edit_data=>c_validation-incorrect_values ).
  ENDMETHOD.

  METHOD on_data_changed.
    zif_zabap_table_edit_tab_data~was_data_changed = abap_true.
  ENDMETHOD.

  METHOD yes_no.
    DATA answer TYPE c LENGTH 1.

    MESSAGE i011(zabap_table_edit) INTO DATA(yes).
    MESSAGE i012(zabap_table_edit) INTO DATA(no).

    CALL FUNCTION 'POPUP_TO_CONFIRM'
      EXPORTING
        titlebar              = title
        text_question         = question
        text_button_1         = yes
        icon_button_1         = '@0V@'  " Okay icon
        text_button_2         = no
        icon_button_2         = '@0W@' " No icon
        default_button        = '1'
        display_cancel_button = ||
        start_column          = start_col
        start_row             = start_row
        popup_type            = 'ICON_MESSAGE_QUESTION' " check ICON_MESSAGE_ in TYPE_POOL Icon
      IMPORTING
        answer                = answer
      EXCEPTIONS
        text_not_found        = 1
        OTHERS                = 2.

    confirmed = xsdbool( answer = '1' ).
  ENDMETHOD.

  METHOD zif_zabap_table_edit_tab_data~restrict_selection.
    MESSAGE |Restrict selection changed='{ changed }'| TYPE 'I'.
  ENDMETHOD.

  METHOD zif_zabap_table_edit_tab_data~switch_tech_display.
    MESSAGE |Switch tech display| TYPE 'I'.
  ENDMETHOD.

ENDCLASS.
