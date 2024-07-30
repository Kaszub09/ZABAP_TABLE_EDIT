CLASS zcl_zabap_table_edit_ex4 DEFINITION PUBLIC CREATE PUBLIC INHERITING FROM zcl_zabap_table_edit_chain_ext.

  PUBLIC SECTION.
    METHODS:
      constructor.
ENDCLASS.

CLASS zcl_zabap_table_edit_ex4 IMPLEMENTATION.

  METHOD constructor.
    super->constructor( ).

    add_all_valid_interfaces( NEW zcl_zabap_table_edit_descr_ext(
      trigger_change_fields = VALUE zcl_zabap_table_edit_descr_ext=>tt_trigger_change_fields( ( |MATNR| ) )
      new_fields            = VALUE #( ( after_field = 'MATNR' type_name = 'MAKT-MAKTX' new_name = 'MATNR_DESCR' ) )
      query                 = VALUE #( select = 'maktx AS MATNR_DESCR' from = 'MAKT' where = 'spras = @sy-langu AND matnr = @row-matnr' )
    ) ).

  ENDMETHOD.

ENDCLASS.
