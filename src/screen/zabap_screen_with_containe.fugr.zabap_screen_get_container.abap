FUNCTION ZABAP_SCREEN_GET_CONTAINER.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  EXPORTING
*"     REFERENCE(CONTAINER) TYPE REF TO  CL_GUI_CONTAINER
*"----------------------------------------------------------------------
  IF NOT gui_container IS BOUND.
    gui_container = NEW #( container_name = c_container_control_name ).
  ENDIF.

  container = gui_container.
ENDFUNCTION.
