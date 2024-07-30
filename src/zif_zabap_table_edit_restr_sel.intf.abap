INTERFACE zif_zabap_table_edit_restr_sel PUBLIC.
      METHODS:
      display IMPORTING will_data_be_lost_on_change TYPE abap_bool RETURNING VALUE(changed) TYPE abap_bool RAISING zcx_zabap_table_edit,
      get_where_cond RETURNING VALUE(where) TYPE rsds_where_tab,
      get_field_ranges RETURNING VALUE(field_ranges) TYPE rsds_frange_t.

ENDINTERFACE.
