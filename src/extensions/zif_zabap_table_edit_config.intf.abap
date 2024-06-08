INTERFACE zif_zabap_table_edit_config
  PUBLIC .
  METHODS:
    "! <p class="shorttext synchronized" lang="en">Change initial config, e.g. disable editing for some users</p>
    change_config CHANGING config TYPE zcl_zabap_table_edit=>t_config,
    "! <p class="shorttext synchronized">Called once, e.g. modify commands / hook up other events</p>
    grid_setup CHANGING grid TYPE REF TO cl_gui_alv_grid.
ENDINTERFACE.
