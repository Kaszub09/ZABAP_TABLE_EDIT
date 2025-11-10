"! <p class="shorttext synchronized">AMC extension. Used to broadcast that the table content has changed (and possibly other messages).
"! <br/>Simply add this class to any ZABAP_TABLE_EDIT transaction, then create and register receiver in another program
"! to be notified about table content changes./p>
CLASS zcl_zabap_table_edit_amc_ext DEFINITION PUBLIC CREATE PUBLIC.

  PUBLIC SECTION.
    CONSTANTS:
      BEGIN OF c_amc,
        application_id          TYPE amc_application_id VALUE 'ZABAP_TABLE_EDIT',
        content_changed_channel TYPE amc_channel_id VALUE '/content_changed',
      END OF c_amc.

    INTERFACES:
      zif_zabap_table_edit_config,
      zif_zabap_table_edit_data.

    INTERFACES:
      if_amc_message_receiver_text.

    METHODS:
      start_receiving RETURNING VALUE(self) TYPE REF TO zcl_zabap_table_edit_amc_ext RAISING cx_amc_error,
      stop_receiving RETURNING VALUE(self) TYPE REF TO zcl_zabap_table_edit_amc_ext RAISING cx_amc_error,
      register_receiver IMPORTING receiver TYPE REF TO zif_zabap_table_edit_amc RETURNING VALUE(self) TYPE REF TO zcl_zabap_table_edit_amc_ext,
      clear_all_receivers RETURNING VALUE(self) TYPE REF TO zcl_zabap_table_edit_amc_ext.

  PRIVATE SECTION.
    DATA:
        table_name TYPE string.

    DATA:
      message_consumer TYPE REF TO if_amc_message_consumer,
      receivers        TYPE STANDARD TABLE OF REF TO zif_zabap_table_edit_amc WITH EMPTY KEY.
ENDCLASS.

CLASS zcl_zabap_table_edit_amc_ext IMPLEMENTATION.
  METHOD zif_zabap_table_edit_config~change_config.
    table_name = config-table_name.
  ENDMETHOD.

  METHOD zif_zabap_table_edit_data~after_save.
    CAST if_amc_message_producer_text( cl_amc_channel_manager=>create_message_producer(
        i_application_id = c_amc-application_id i_channel_id = c_amc-content_changed_channel )
       )->send( i_message = table_name ).
  ENDMETHOD.

  METHOD if_amc_message_receiver_text~receive.
    LOOP AT receivers INTO DATA(receiver).
      receiver->table_content_changed( i_message ).
    ENDLOOP.
  ENDMETHOD.

  METHOD start_receiving.
    self = me.
    IF message_consumer IS NOT BOUND.
      message_consumer = cl_amc_channel_manager=>create_message_consumer(
        i_application_id = c_amc-application_id i_channel_id = c_amc-content_changed_channel ).
    ENDIF.
    message_consumer->start_message_delivery( me ).
  ENDMETHOD.

  METHOD stop_receiving.
    self = me.
    IF message_consumer IS NOT BOUND.
      RETURN.
    ENDIF.
    message_consumer->stop_message_delivery( me ).
  ENDMETHOD.

  METHOD register_receiver.
    self = me.
    APPEND receiver TO receivers.
  ENDMETHOD.

  METHOD clear_all_receivers.
    self = me.
    CLEAR: receivers.
  ENDMETHOD.
ENDCLASS.
