*&---------------------------------------------------------------------*
*& Report zabap_table_edit_dupli_test
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zabap_table_edit_dupli_test.

DATA(messages) = NEW zcl_zabap_table_edit_messages( ).

SELECT * FROM dd03l WHERE tabname = 'ZABAP_TE_TEST' INTO TABLE @DATA(test_data).
SELECT * FROM dd03l WHERE tabname = 'ZABAP_TE_TEST' APPENDING CORRESPONDING FIELDS OF TABLE @test_data.

SORT test_data BY tabname fieldname.

messages->show_data( msg = |Duplicates| table_name = 'ZABAP_TE_TEST' data_table = REF #( test_data ) ).
