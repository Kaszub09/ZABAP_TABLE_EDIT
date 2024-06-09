CLASS zcl_zabap_table_edit_cd_test DEFINITION PUBLIC CREATE PUBLIC FOR TESTING.

  PUBLIC SECTION.
    INTERFACES:
      zif_zabap_change_document.

    METHODS:
      constructor IMPORTING change_doc_type TYPE zabap_change_doc_type.

  PRIVATE SECTION.
    METHODS:
      fail_if_cd_disabled.

    DATA:
      change_doc TYPE zabap_change_doc_type,
      is_open    TYPE abap_bool VALUE abap_false.
ENDCLASS.

CLASS zcl_zabap_table_edit_cd_test IMPLEMENTATION.
  METHOD constructor.
    change_doc = change_doc_type.
  ENDMETHOD.

  METHOD zif_zabap_change_document~open.
    fail_if_cd_disabled( ).
    IF is_open = abap_true.
      cl_abap_unit_assert=>fail( |CD opened second time without close| ).
    ENDIF.
    is_open = abap_true.
  ENDMETHOD.

  METHOD zif_zabap_change_document~close.
    fail_if_cd_disabled( ).
    IF is_open = abap_false.
      cl_abap_unit_assert=>fail( |CD closed without being opened| ).
    ENDIF.
    is_open = abap_false.
  ENDMETHOD.

  METHOD zif_zabap_change_document~change_single.
    fail_if_cd_disabled( ).
    fail_if_cd_disabled( ).
    IF is_open = abap_false.
      cl_abap_unit_assert=>fail( |CD changed without being opened| ).
    ENDIF.
  ENDMETHOD.

  METHOD zif_zabap_change_document~change_multi.
    fail_if_cd_disabled( ).
    IF is_open = abap_false.
      cl_abap_unit_assert=>fail( |CD changed without being opened| ).
    ENDIF.
  ENDMETHOD.

  METHOD zif_zabap_change_document~create_table_with_indicator.
  ENDMETHOD.

  METHOD fail_if_cd_disabled.
    IF change_doc = space.
      cl_abap_unit_assert=>fail( |CD was disabled and shouldn't be called| ).
    ENDIF.
  ENDMETHOD.
ENDCLASS.
