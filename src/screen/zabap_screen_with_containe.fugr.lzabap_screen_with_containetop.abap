FUNCTION-POOL ZABAP_SCREEN_WITH_CONTAINE.   "MESSAGE-ID ..

* INCLUDE LZABAP_SCREEN_WITH_CONTAINED...    " Local class definition

CONSTANTS:
    c_container_control_name TYPE c LENGTH 9 VALUE 'CONTAINER'.

DATA:
  gui_container                TYPE REF TO cl_gui_custom_container,
  status_to_display            TYPE string VALUE 'DYNAMIC_COMMANDS',
  header_text_to_display       TYPE string,
  commands_to_exclude_from_top TYPE ztt_zabap_commands,
  was_screen_called            TYPE abap_bool VALUE abap_false.

DATA:
  BEGIN OF dynamic,
    dynamic_1  TYPE smp_dyntxt,
    dynamic_2  TYPE smp_dyntxt,
    dynamic_3  TYPE smp_dyntxt,
    dynamic_4  TYPE smp_dyntxt,
    dynamic_5  TYPE smp_dyntxt,
    dynamic_6  TYPE smp_dyntxt,
    dynamic_7  TYPE smp_dyntxt,
    dynamic_8  TYPE smp_dyntxt,
    dynamic_10 TYPE smp_dyntxt,
    dynamic_11 TYPE smp_dyntxt,
    dynamic_12 TYPE smp_dyntxt,
    dynamic_13 TYPE smp_dyntxt,
    dynamic_14 TYPE smp_dyntxt,
    dynamic_15 TYPE smp_dyntxt,
    dynamic_16 TYPE smp_dyntxt,
    dynamic_17 TYPE smp_dyntxt,
    dynamic_18 TYPE smp_dyntxt,
    dynamic_19 TYPE smp_dyntxt,
    dynamic_20 TYPE smp_dyntxt,
    dynamic_21 TYPE smp_dyntxt,
    dynamic_22 TYPE smp_dyntxt,
    dynamic_23 TYPE smp_dyntxt,
    dynamic_24 TYPE smp_dyntxt,
    dynamic_25 TYPE smp_dyntxt,
    dynamic_26 TYPE smp_dyntxt,
    dynamic_27 TYPE smp_dyntxt,
    dynamic_28 TYPE smp_dyntxt,
    dynamic_29 TYPE smp_dyntxt,
    dynamic_30 TYPE smp_dyntxt,
    dynamic_31 TYPE smp_dyntxt,
    dynamic_32 TYPE smp_dyntxt,
    dynamic_33 TYPE smp_dyntxt,
    dynamic_34 TYPE smp_dyntxt,
    dynamic_35 TYPE smp_dyntxt,
  END OF dynamic.
