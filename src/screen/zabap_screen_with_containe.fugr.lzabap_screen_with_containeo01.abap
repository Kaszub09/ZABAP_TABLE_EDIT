*----------------------------------------------------------------------*
***INCLUDE LZABAP_SCREEN_WITH_CONTAINEO01.
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Module  STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status OUTPUT.
  SET PF-STATUS status_to_display EXCLUDING commands_to_exclude_from_top.
  IF strlen( header_text_to_display ) > 0.
    SET TITLEBAR 'HEADER' WITH header_text_to_display.
  ENDIF.
  was_screen_called = abap_true.
ENDMODULE.
