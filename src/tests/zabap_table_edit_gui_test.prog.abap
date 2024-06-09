*&---------------------------------------------------------------------*
*& Report zabap_table_edit_gui_test
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zabap_table_edit_gui_test.
PARAMETERS:
  p_discdv AS CHECKBOX,
  p_disedi AS CHECKBOX.

START-OF-SELECTION.
  zcl_zabap_table_edit_fact_inj=>inject_table_data( NEW zcl_zabap_te_tab_data_test( ) ).
  DATA(table_edit) = NEW zcl_zabap_table_edit( VALUE #( disable_cd_view = p_discdv disable_editing = p_disedi ) ).
  table_edit->display( ).
