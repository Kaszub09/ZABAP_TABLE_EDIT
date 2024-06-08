INTERFACE zif_zabap_change_document PUBLIC.
  TYPES:
    "! Decide whether to save individual fields on deletion/insertion
    BEGIN OF t_save_fields,
      "! Save fields when deleting (otherwise only key)
      docu_delete    TYPE abap_bool,
      "! Save fields when inserting (otherwise only key)
      docu_insert    TYPE abap_bool,
      "! Save all fields when deleting (otherwise only non-initial)
      docu_delete_if TYPE abap_bool,
      "! Save all fields when inserting (otherwise only non-initial)
      docu_insert_if TYPE abap_bool,
    END OF t_save_fields.

  CONSTANTS:
    BEGIN OF c_save_all,
      docu_delete    TYPE abap_bool VALUE abap_true,
      docu_insert    TYPE abap_bool VALUE abap_true,
      docu_delete_if TYPE abap_bool VALUE abap_true,
      docu_insert_if TYPE abap_bool VALUE abap_true,
    END OF c_save_all,
    BEGIN OF c_save_none,
      docu_delete    TYPE abap_bool VALUE abap_false,
      docu_insert    TYPE abap_bool VALUE abap_false,
      docu_delete_if TYPE abap_bool VALUE abap_false,
      docu_insert_if TYPE abap_bool VALUE abap_false,
    END OF c_save_none,
    BEGIN OF c_save_non_initial,
      docu_delete    TYPE abap_bool VALUE abap_true,
      docu_insert    TYPE abap_bool VALUE abap_true,
      docu_delete_if TYPE abap_bool VALUE abap_false,
      docu_insert_if TYPE abap_bool VALUE abap_false,
    END OF c_save_non_initial.

  METHODS:
    "! Must be called first, before any changes
    open RAISING zcx_zabap_table_edit,
    "! <p class="shorttext synchronized">CD must be opened first</p>
    "! @parameter table_name | <p class="shorttext synchronized">Must be valid DDIC table</p>
    "! @parameter force_cd_on_all_fields | <p class="shorttext synchronized">Create CD even on Data Elements without CD logflag</p>
    "! @parameter inserted | <p class="shorttext synchronized">Must be ref to structure of type <em>table_name</em></p>
    "! @parameter deleted | <p class="shorttext synchronized">Must be ref to structure of type <em>table_name</em></p>
    "! @parameter before_modified | <p class="shorttext synchronized">Must be ref to structure of type <em>table_name</em></p>
    "! @parameter modified | <p class="shorttext synchronized">Must be ref to structure of type <em>table_name</em></p>
    "! @parameter save_fields | <p class="shorttext synchronized">Whether to save individual fields on deletion or insertion</p>
    change_single IMPORTING table_name TYPE string force_cd_on_all_fields TYPE abap_bool DEFAULT abap_false
                            inserted TYPE REF TO data OPTIONAL deleted TYPE REF TO data OPTIONAL
                            before_modified TYPE REF TO data OPTIONAL modified TYPE REF TO data OPTIONAL
                            save_fields TYPE t_save_fields DEFAULT c_save_non_initial
                            RAISING zcx_zabap_table_edit,
    "! <p class="shorttext synchronized">CD must be opened first. All tables must be sorted.</p>
    "! @parameter table_name | <p class="shorttext synchronized">Must be valid DDIC table</p>
    "! @parameter force_cd_on_all_fields | <p class="shorttext synchronized">Create CD even on Data Elements without CD logflag</p>
    "! @parameter inserted | <p class="shorttext synchronized">Must be ref to table <em>table_name</em> with c field</p>
    "! @parameter deleted | <p class="shorttext synchronized">Must be ref to table <em>table_name</em> with c field</p>
    "! @parameter before_modified | <p class="shorttext synchronized">Must be ref to table <em>table_name</em> with c field</p>
    "! @parameter modified | <p class="shorttext synchronized">Must be ref to table <em>table_name</em> with c field</p>
    "! @parameter save_fields | <p class="shorttext synchronized">Whether to save individual fields on deletion or insertion</p>
    change_multi IMPORTING table_name TYPE string force_cd_on_all_fields TYPE abap_bool DEFAULT abap_false
                           inserted TYPE REF TO data OPTIONAL deleted TYPE REF TO data OPTIONAL
                           before_modified TYPE REF TO data OPTIONAL modified TYPE REF TO data OPTIONAL
                           save_fields TYPE t_save_fields DEFAULT c_save_non_initial
                           RAISING zcx_zabap_table_edit,
    "! <p class="shorttext synchronized">Converts table to be used with <em>change_multi</em></p>
    "! @parameter table_name | <p class="shorttext synchronized">Must be valid DDIC table</p>
    "! @parameter original_table | <p class="shorttext synchronized">Must be ref to table <em>table_name</em></p>
    "! @parameter indicator | <p class="shorttext synchronized">Value of added c field (D, I, U or space)</p>
    "! @parameter sort | <p class="shorttext synchronized">Sort table by key fields</p>
    "! @parameter table_with_indicator | <p class="shorttext synchronized">Ref to table <em>table_name</em> with added c field</p>
    "! @parameter indicator_field_name | <p class="shorttext synchronized">Name of added c field</p>
    create_table_with_indicator IMPORTING table_name TYPE string original_table TYPE REF TO data indicator TYPE c DEFAULT space
                                          sort TYPE abap_bool DEFAULT abap_false
                                EXPORTING indicator_field_name TYPE string
                                RETURNING VALUE(table_with_indicator) TYPE REF TO data,
    "! <p class="shorttext synchronized">CD must be opened first</p>
    "! @parameter object_change_indicator | <p class="shorttext synchronized">For header entry only</p>
    "! @parameter skip_exception_if_no_changes | <p class="shorttext synchronized">Don't raise exception if no position inserted</p>
    close IMPORTING date_of_change TYPE d DEFAULT sy-datum tcode TYPE syst_tcode DEFAULT sy-tcode time_of_change TYPE t DEFAULT sy-uzeit
                    username TYPE syst_uname DEFAULT sy-uname object_change_indicator TYPE cdchngindh DEFAULT 'U'
                    skip_exception_if_no_changes TYPE abap_bool DEFAULT abap_false
          RETURNING VALUE(changenumber) TYPE cdchangenr
          RAISING zcx_zabap_table_edit.
ENDINTERFACE.
