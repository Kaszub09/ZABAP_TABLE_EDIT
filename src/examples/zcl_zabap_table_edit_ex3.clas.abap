CLASS zcl_zabap_table_edit_ex3 DEFINITION PUBLIC CREATE PUBLIC.

  PUBLIC SECTION.
    INTERFACES:
      zif_zabap_table_edit_config.
ENDCLASS.

CLASS zcl_zabap_table_edit_ex3 IMPLEMENTATION.
  METHOD zif_zabap_table_edit_config~change_config.
    DATA(hide_change_doc_for_user) = sy-uname.
    IF sy-uname = hide_change_doc_for_user.
      config-disable_cd_view = abap_true.
    ENDIF.
  ENDMETHOD.

  METHOD zif_zabap_table_edit_config~grid_setup.
  ENDMETHOD.
ENDCLASS.
