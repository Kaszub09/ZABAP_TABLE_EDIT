*&---------------------------------------------------------------------*
*& Report zabap_table_edit
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zabap_table_edit.

PARAMETERS: p_tabnam TYPE string DEFAULT ''.
PARAMETERS: p_cd TYPE ZABAP_CHANGE_DOC_TYPE DEFAULT 'X'.
PARAMETERS: p_class TYPE string DEFAULT ''.

START-OF-SELECTION.
  "Check if it's correct table
  SELECT SINGLE tabclass  FROM dd02l WHERE tabname = @p_tabnam INTO @DATA(tabclass).
  IF tabclass  <> 'TRANSP'.
    MESSAGE TEXT-001 TYPE 'S' DISPLAY LIKE 'E'.
    LEAVE LIST-PROCESSING.
  ENDIF.

  "Get description
  DATA original_tcode TYPE tcode.
  CALL 'GET_PARAM_TCOD' ID 'PTCOD' FIELD original_tcode.
  SELECT SINGLE ttext FROM tstct WHERE sprsl = @sy-langu AND tcode = @original_tcode INTO @DATA(description).


  DATA(table_edit) = NEW zcl_zabap_table_edit( table_name = p_tabnam class_name = p_class header_text = CONV #( description ) ).
  table_edit->set_change_doc_type( p_cd ).
  table_edit->display( ).
