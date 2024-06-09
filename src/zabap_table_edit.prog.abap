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
  p_distt  AS CHECKBOX.

START-OF-SELECTION.
  "Check if it's correct table
  SELECT SINGLE tabclass, viewclass FROM dd02l WHERE tabname = @p_tabnam INTO @DATA(tab_info).
  IF tab_info-tabclass <> 'TRANSP' AND ( tab_info-tabclass <> 'VIEW' OR tab_info-viewclass <> 'C' ).
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
          extensions-commands = CAST #( extension_inst ).
        ENDIF.
        IF extension_inst IS INSTANCE OF zif_zabap_table_edit_config.
          extensions-config = CAST #( extension_inst ).
        ENDIF.
        IF extension_inst IS INSTANCE OF zif_zabap_table_edit_data.
          extensions-data = CAST #( extension_inst ).
        ENDIF.
      CATCH cx_sy_create_object_error INTO DATA(create_object_error).
        MESSAGE create_object_error TYPE 'S' DISPLAY LIKE 'E'.
        RETURN.
    ENDTRY.
  ENDIF.

  DATA(table_edit) = NEW zcl_zabap_table_edit( VALUE #( display_text = description view_name = p_tabnam change_doc_type = p_cd
    disable_cd_view = p_discdv disable_editing = p_disedi disable_text_table = p_distt ext = extensions ) ).
  table_edit->display( ).
