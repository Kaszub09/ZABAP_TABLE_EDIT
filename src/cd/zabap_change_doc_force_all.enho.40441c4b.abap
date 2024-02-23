"Name: \PR:SAPLSCD0\FO:ASSIGN_STRUCTURE\SE:END\EI
ENHANCEMENT 0 ZABAP_CHANGE_DOC_FORCE_ALL.
    DATA zabap_table_name TYPE string.
    DATA(zabap_force_logging) = abap_false.
    IMPORT zabap_force_logging = zabap_force_logging zabap_table_name = zabap_table_name
    FROM MEMORY ID zcl_zabap_change_document=>c_change_doc_force_log_all_fie.
    IF sy-subrc = 0 AND zabap_force_logging = abap_true.
      LOOP AT tabinfo[] REFERENCE INTO DATA(tabinfo_field_ref) WHERE tabname = zabap_table_name AND logflag = abap_false AND keyflag = abap_false.
        tabinfo_field_ref->logflag = 'F'.
      ENDLOOP.
    ENDIF.
ENDENHANCEMENT.
