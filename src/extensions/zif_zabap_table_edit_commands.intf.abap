INTERFACE zif_zabap_table_edit_commands PUBLIC.
  METHODS:
    "! <p class="shorttext synchronized">Called whenever edit mode is changed</p>
    set_edit_mode DEFAULT IGNORE CHANGING editable TYPE abap_bool,
    "! <p class="shorttext synchronized">Can execute custom command and alter default behavior</p>
    "! @parameter command | <p class="shorttext synchronized">Can be modified to alter called command</p>
    "! @parameter cancel_command | <p class="shorttext synchronized">Doesn't execute command</p>
    before_command DEFAULT IGNORE CHANGING command TYPE syst_ucomm cancel_command TYPE abap_bool,
    "! <p class="shorttext synchronized">Not called if command was cancelled</p>
    after_command DEFAULT IGNORE CHANGING command TYPE syst_ucomm,
    "! <p class="shorttext synchronized" lang="en">Called after setting up the screen</p>
    "! @parameter commands | <p class="shorttext synchronized">Can be modified to alter displayed commands</p>
    change_commands DEFAULT IGNORE IMPORTING in_edit_mode TYPE abap_bool CHANGING commands TYPE ztt_zabap_commands.
ENDINTERFACE.
