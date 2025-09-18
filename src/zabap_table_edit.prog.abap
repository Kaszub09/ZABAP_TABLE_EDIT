*&---------------------------------------------------------------------*
*& Report zabap_table_edit
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zabap_table_edit.

PARAMETERS:
  p_tabnam TYPE string,
  p_cd     TYPE zabap_change_doc_type DEFAULT 'X',
  p_class  TYPE string,
  p_discdv AS CHECKBOX,
  p_disedi AS CHECKBOX,
  p_distt  AS CHECKBOX,
  p_dissel AS CHECKBOX DEFAULT abap_true,
  p_asksel AS CHECKBOX,
  p_distec AS CHECKBOX DEFAULT abap_true,
  p_doccla TYPE doku_class,
  p_docnam TYPE string.


START-OF-SELECTION.
  "Check if it's correct table
  SELECT SINGLE tabclass FROM dd02l WHERE tabname = @p_tabnam INTO @DATA(tabclass).
  IF tabclass = 'VIEW'.
    p_disedi = abap_true.
  ELSEIF tabclass <> 'TRANSP'.
    MESSAGE s001(zabap_table_edit) DISPLAY LIKE 'E'.
    LEAVE LIST-PROCESSING.
  ENDIF.

  "Get description
  DATA original_tcode TYPE tcode.
  CALL 'GET_PARAM_TCOD' ID 'PTCOD' FIELD original_tcode.
  SELECT SINGLE ttext FROM tstct WHERE sprsl = @sy-langu AND tcode = @original_tcode INTO @DATA(description).

  "Create extensions
  DATA extensions TYPE zcl_zabap_table_edit=>t_config-ext.
  IF p_class IS NOT INITIAL.
    TRY.
        DATA extension_inst TYPE REF TO object.
        CREATE OBJECT extension_inst TYPE (p_class).
        IF extension_inst IS INSTANCE OF zif_zabap_table_edit_commands.
          APPEND CAST #( extension_inst ) TO extensions-commands.
        ENDIF.
        IF extension_inst IS INSTANCE OF zif_zabap_table_edit_config.
          APPEND CAST #( extension_inst ) TO extensions-config.
        ENDIF.
        IF extension_inst IS INSTANCE OF zif_zabap_table_edit_data.
          APPEND CAST #( extension_inst ) TO extensions-data.
        ENDIF.
      CATCH cx_sy_create_object_error INTO DATA(create_object_error).
        MESSAGE create_object_error TYPE 'S' DISPLAY LIKE 'E'.
        RETURN.
    ENDTRY.
  ENDIF.

  DATA(table_edit) = NEW zcl_zabap_table_edit( VALUE #( display_text = description table_name = p_tabnam change_doc_type = p_cd
    disable_cd_view = p_discdv disable_editing = COND #( WHEN to_upper( p_tabnam(1) ) CA 'YZ' THEN p_disedi ELSE abap_true )
    disable_text_table = p_distt ext = extensions disable_selection = p_dissel show_selection_first = p_asksel
    disable_switch_tech_display = p_distec
    documentation = VALUE #( name = p_docnam class = p_doccla ) ) ).
  table_edit->display( ).
