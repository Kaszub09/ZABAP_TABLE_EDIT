CLASS zcl_zabap_table_edit_lock DEFINITION PUBLIC CREATE PUBLIC.

  PUBLIC SECTION.
    METHODS:
      constructor IMPORTING table_name TYPE string,
      lock_table EXPORTING error_message TYPE string RETURNING VALUE(locked) TYPE abap_bool,
      unlock_table.

  PRIVATE SECTION.
    DATA:
      table_name TYPE tabname.
ENDCLASS.

CLASS zcl_zabap_table_edit_lock IMPLEMENTATION.
  METHOD constructor.
    me->table_name = CONV #( table_name ).
  ENDMETHOD.

  METHOD lock_table.
    CALL FUNCTION 'ENQUEUE_E_TABLE'
      EXPORTING
        mode_rstable   = 'E'
        tabname        = table_name
        x_tabname      = space
        x_varkey       = space
        _scope         = '2'
        _wait          = space
        _collect       = ' '
      EXCEPTIONS
        foreign_lock   = 1
        system_failure = 2
        OTHERS         = 3.

    locked = COND #( WHEN sy-subrc = 0 THEN abap_true ELSE abap_false ).
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 INTO error_message.
    ENDIF.
  ENDMETHOD.

  METHOD unlock_table.
    CALL FUNCTION 'DEQUEUE_E_TABLE'
      EXPORTING
        mode_rstable = 'E'
        tabname      = table_name
        x_tabname    = space
        x_varkey     = space
        _scope       = '2'
        _synchron    = space
        _collect     = ' '.
  ENDMETHOD.
ENDCLASS.
