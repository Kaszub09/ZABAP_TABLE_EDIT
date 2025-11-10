"! <p class="shorttext synchronized" lang="en">AMC messages</p>
INTERFACE zif_zabap_table_edit_amc PUBLIC.
  METHODS:
    table_content_changed IMPORTING table_name TYPE string.
ENDINTERFACE.
