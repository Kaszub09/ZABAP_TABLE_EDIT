*&---------------------------------------------------------------------*
*& Report zabap_table_edit_ex5
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zabap_table_edit_ex5.
"It could be used, for example, when performing some check during save.
"E.g. we check during purchase order save if some field value exists in control table.
"   - On one hand, we want to cache check for performance (avoid trip to database every time check is called).
"   - On the other hand, if user modifies control table we don'w want to force him to exit and enter current transaction again.
"     Especially, if it would involve loosing some data because check doesn't allow to save.
"The solution is to detect control table data changes and invalidate cache.

"Open this program in one session and modify ZABAP_TAB_ED_EX5 via ZABAP_TAB_ED_EX5 transaction in another.

"--------------------------------------------------
"Class imitating simple cache behaviour on table ZABAP_TAB_ED_EX5
CLASS lcl_zabap_tab_ed_ex5_cache DEFINITION CREATE PUBLIC.

  PUBLIC SECTION.
    INTERFACES:
      zif_zabap_table_edit_amc.

    METHODS:
      matnr_exists IMPORTING matnr TYPE matnr RETURNING VALUE(exists) TYPE abap_bool,
      purge_cache.

  PRIVATE SECTION.
    TYPES:
      BEGIN OF t_matnr_exists,
        matnr  TYPE matnr,
        exists TYPE abap_bool,
      END OF t_matnr_exists,
      tt_matnr_exists TYPE SORTED TABLE OF t_matnr_exists WITH UNIQUE KEY matnr.

    DATA:
        cache TYPE tt_matnr_exists.
ENDCLASS.

CLASS lcl_zabap_tab_ed_ex5_cache IMPLEMENTATION.
  METHOD zif_zabap_table_edit_amc~table_content_changed.
    "When table content is changed, purge cache. Alternatively, you could pass more detailed information about records changed.
    IF table_name = 'ZABAP_TAB_ED_EX5'.
      purge_cache( ).
    ENDIF.
  ENDMETHOD.

  METHOD matnr_exists.
    DATA(matnr_ref) = REF #( cache[ matnr = matnr ] OPTIONAL ).
    IF matnr_ref IS NOT BOUND.
      matnr_ref = NEW #( matnr = matnr ).
      SELECT SINGLE @abap_true FROM zabap_tab_ed_ex5 WHERE matnr = @matnr INTO @matnr_ref->exists.
      INSERT matnr_ref->* INTO TABLE cache.
    ENDIF.
    exists = matnr_ref->exists.
  ENDMETHOD.

  METHOD purge_cache.
    CLEAR: cache.
  ENDMETHOD.
ENDCLASS.

"--------------------------------------------------
TABLES: sscrfields.
SELECTION-SCREEN FUNCTION KEY 1.

PARAMETERS:
    p_matnr TYPE matnr.

INITIALIZATION.
  sscrfields-functxt_01 = 'Check matnr exists in ZABAP_TAB_ED_EX5'.
  "Create cache, then register it and start listening
  DATA(lcl_cache) = NEW lcl_zabap_tab_ed_ex5_cache( ).
  DATA(amc) = NEW zcl_zabap_table_edit_amc_ext( )->register_receiver( lcl_cache )->start_receiving( ).

AT SELECTION-SCREEN.
  IF sy-ucomm = 'FC01'.
    MESSAGE |Exists = '{ lcl_cache->matnr_exists( p_matnr ) }'| TYPE 'I'.
  ENDIF.
