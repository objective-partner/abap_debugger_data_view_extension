CLASS ltc_table_enh_should DEFINITION DEFERRED.
CLASS zcl_op_table   DEFINITION LOCAL FRIENDS ltc_table_enh_should.

CLASS ltc_table_enh_should DEFINITION FOR TESTING
  DURATION SHORT  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    DATA:
      table_view TYPE REF TO zcl_op_table.  "class under test

    METHODS:
      setup,
      teardown,
      "!check of
      prepare_ouput_w_1_line_itab FOR TESTING,
      structure_in_itab           FOR TESTING,
      itab_in_itab                FOR TESTING,
      st05_main_record_table FOR TESTING RAISING cx_static_check,
      ris_t_metadata FOR TESTING RAISING cx_static_check.
ENDCLASS.


CLASS ltc_table_enh_should IMPLEMENTATION.

  METHOD setup.
    table_view = NEW zcl_op_table( ).
  ENDMETHOD.


  METHOD teardown.
    CLEAR table_view.
  ENDMETHOD.


  METHOD prepare_ouput_w_1_line_itab.

    TYPES: BEGIN OF ty_sflight_mini,
             carrid TYPE s_carr_id,
             connid TYPE s_conn_id,
             fldate TYPE s_date,
             price  TYPE s_price,
           END OF ty_sflight_mini.
    DATA: output        TYPE                   string,
          output_exp    TYPE                   string,
          sflight_lines TYPE STANDARD TABLE OF ty_sflight_mini.


    sflight_lines = VALUE #(  ( carrid = 'AA'   connid = '0017' fldate = '20170810' price = '422.94 ' ) ).

    output = table_view->prepare_output(  i_table          = sflight_lines
                                          i_table_title    = 'SFLIGHT_LINES' ).

    output_exp = |SFLIGHT_LINES = VALUE #( ( CARRID = 'AA' CONNID = '0017' FLDATE = '20170810' PRICE = '422.94 ' ) ).|.
    cl_abap_unit_assert=>assert_equals( EXPORTING  act = output
                                                   exp = output_exp
                                                   msg = |Character String Different| ).


  ENDMETHOD.

  METHOD itab_in_itab.
    TYPES: BEGIN OF ts_flight_data,
             carrid TYPE s_carr_id,
             connid TYPE s_conn_id,
             fldate TYPE s_date,
             price  TYPE s_price,
           END OF ts_flight_data,
           tt_flight_data TYPE STANDARD TABLE OF ts_flight_data WITH EMPTY KEY,
           BEGIN OF ts_flight_data_deep,
             col1 TYPE tt_flight_data,
           END OF ts_flight_data_deep,
           tt_flight_data_deep TYPE STANDARD TABLE OF ts_flight_data_deep WITH EMPTY KEY.

    DATA:  output         TYPE string,
           output_exp     TYPE string,
           sflight_lines  TYPE tt_flight_data_deep.


    sflight_lines = VALUE #( ( col1 = VALUE #(
                                            ( carrid = 'AA'   connid = '0017' fldate = '20170810' price = '422.94 ' )
                                            ( carrid = 'AA'   connid = '0017' fldate = '20170810' price = '422.94 ' )
                                          )
                          )
                          ( col1 = VALUE #(
                                            ( carrid = 'AA'   connid = '0017' fldate = '20170810' price = '422.94 ' )
                                            ( carrid = 'AA'   connid = '0017' fldate = '20170810' price = '422.94 ' )
                                          )
                          )
                        ).


    output = table_view->prepare_output( i_table          = sflight_lines
                                                       i_table_title    = 'SFLIGHT_LINES' ).

    output_exp = |SFLIGHT_LINES = VALUE #( ( COL1 = VALUE #( ( CARRID = 'AA' CONNID = '0017' FLDATE = '20170810' PRICE = '422.94 ' )| &&
                 | ( CARRID = 'AA' CONNID = '0017' FLDATE = '20170810' PRICE = '422.94 ' ) ) ) ( COL1 = VALUE #( ( CARRID = 'AA' CONNID| &&
                 | = '0017' FLDATE = '20170810' PRICE = '422.94 ' ) ( CARRID = 'AA' CONNID = '0017' FLDATE = '20170810' PRICE = '422.94 ' ) ) ) ).|.

    cl_abap_unit_assert=>assert_equals( EXPORTING act = output
                                                  exp = output_exp
                                                  msg = |Itab in itab formating is not as expected|
                                                  quit = if_aunit_constants=>method ).

  ENDMETHOD.


  METHOD structure_in_itab.

    TYPES: BEGIN OF ts_flight_data,
             carrid TYPE s_carr_id,
             connid TYPE s_conn_id,
             fldate TYPE s_date,
             price  TYPE s_price,
           END OF ts_flight_data,
           BEGIN OF ts_flight_data_w_structure,
             col1 TYPE ts_flight_data,
           END OF ts_flight_data_w_structure,
           tt_flight_data_w_structure TYPE STANDARD TABLE OF ts_flight_data_w_structure WITH EMPTY KEY.


    DATA: output        TYPE string,
          output_exp    TYPE string,
          sflight_lines TYPE tt_flight_data_w_structure. "ztt_age_col1


    sflight_lines = VALUE #( ( col1 = VALUE #( carrid = 'AA' connid = '0017' fldate = '20170810' price = '422.94 ' )
                          )
                          ( col1 = VALUE #( carrid = 'AA' connid = '0017' fldate = '20170810' price = '422.94 ' )
                          )
                        ).


    output = table_view->prepare_output( i_table = sflight_lines
                                                       i_table_title = 'SFLIGHT_LINES' ).

    output_exp = |SFLIGHT_LINES = VALUE #( ( COL1 = VALUE #( CARRID = 'AA' CONNID = '0017' FLDATE = '20170810' PRICE = '422.94' ) )| &&
                 | ( COL1 = VALUE #( CARRID = 'AA' CONNID = '0017' FLDATE = '20170810' PRICE = '422.94' ) ) ).|.

    cl_abap_unit_assert=>assert_equals( EXPORTING act = output
                                                  exp = output_exp
                                                  msg = |Structure in itab formating is not as expected|
                                                  quit = if_aunit_constants=>method ).
  ENDMETHOD.

  METHOD ST05_MAIN_RECORD_TABLE.

  data(table) = value st05_main_record_table(
    ( date                           = sy-datum
      time                           = sy-uzeit
      instance_name                  = 'INSTANCE_NAME'
      duration                       = '10.5'
      number_of_rows                 = '11.5'
      object                         = 'OBJECT'
      statement_with_values          = `STATEMENT_WITH_VALUES`
      cursor                         = 'CURSOR'
      array_size                     = 12
      transaction                    = 'TRANSACTION'
      program                        = 'PROGRAM'
      db_connection_name             = 'DB_CONNECTION_NAME'
      db_connection_id               = 'DB_CON_ID'
      operation                      = 'OPERATI'
      return_code                    = 13
      wp_id                          = '001'
      wp_type                        = 'DIA'
      user_name                      = 'USER_NAME'
      client                         = '100'
      trans_id                       = 'TRANS_ID'
      epp_root_id                    = 'EPP_ROOT_ID'
      epp_connection_id              = 'EPP_CONNECTION_ID'
      epp_connection_counter         = 14
      record_number                  = 15
      record_numbers                 = VALUE #( ( 16 ) ( 17 ) )
      offset                         = 18
      statement_with_names           = `STATEMENT_WITH_NAMES`
      length_of_statement_with_names = 19
      variables                      = `VARIABLES`
      number_of_variables            = 20
      trace_type                     = 'TRAC'
      line_color                     = 'LINE'
  ) ).

    data(output) = table_view->prepare_output( i_table       = table
                                         i_table_title = 'TABLE' ).

  ENDMETHOD.


  METHOD ris_t_metadata.
    data(table) = value ris_t_metadata(
      ( trobjtype                  = '????'
        subtype                    = ''
        legacy_type                = 'DWY'
        search_groups              = VALUE ris_t_md_screen_groups(
                                          ( name         = 'STANDARD'
                                            parent_group = ''
                                            text_ref     = 'TEXT-S01' )
                                          ( name         = 'EINSTELLUNG'
                                            parent_group = ''
                                            text_ref     = '' ) )
        search_elements            = VALUE ris_t_md_screen_elements(
                                          ( index     = 1
                                            name      = 'KEY1'
                                            type      = 'SO'
                                            group     = 'STANDARD'
                                            line      = 0
                                            for       = 'SDOKMEP-PROP_NAME'
                                            rb_group  = ''
                                            label_for = ''
                                            label_ref = '' )
                                          ( index     = 2
                                            name      = 'XTEXT'
                                            type      = 'SO'
                                            group     = 'STANDARD'
                                            line      = 0
                                            for       = 'SDOKMET-DESCRIPT'
                                            rb_group  = ''
                                            label_for = ''
                                            label_ref = '' ) )
        where_used                 = VALUE ris_t_md_relationship(
                                          ( trobjtype   = 'ECTC'
                                            subtype     = ''
                                            legacy_type = 'GC'
                                            group       = 1 )
                                          ( trobjtype   = 'ECTC'
                                            subtype     = ''
                                            legacy_type = 'GW'
                                            group       = 1 ) )
        environment                = VALUE ris_t_md_relationship(
                                          ( trobjtype   = 'FUNC'
                                            subtype     = ''
                                            legacy_type = 'FF'
                                            group       = 1 )
                                          ( trobjtype   = 'DIAL'
                                            subtype     = ''
                                            legacy_type = 'A'
                                            group       = 1 ) )
        model_implementations      = VALUE ris_t_md_model_implementation( )
*      ( type_name              =
*        class_name             =
*        priority               =
*        default_implementation = ) )
        where_used_original_dynpro = VALUE ris_t_md_relationship(
                                          ( trobjtype   = 'ECAT'
                                            subtype     = ''
                                            legacy_type = 'GE'
                                            group       = 1 )
                                          ( trobjtype   = 'ECTC'
                                            subtype     = ''
                                            legacy_type = 'GW'
                                            group       = 1 ) )
)
      ( trobjtype                  = '????'
        subtype                    = ''
        legacy_type                = 'GP'
        search_groups              = VALUE ris_t_md_screen_groups(
                                          ( name         = 'STANDARD'
                                            parent_group = ''
                                            text_ref     = 'TEXT-S01' )
                                          ( name         = 'EINSTELLUNG'
                                            parent_group = ''
                                            text_ref     = '' ) )
        search_elements            = VALUE ris_t_md_screen_elements(
                                          ( index     = 1
                                            name      = 'KEY1'
                                            type      = 'SO'
                                            group     = 'STANDARD'
                                            line      = 0
                                            for       = 'SDOKMEP-PROP_NAME'
                                            rb_group  = ''
                                            label_for = ''
                                            label_ref = '' )
                                          ( index     = 2
                                            name      = 'XTEXT'
                                            type      = 'SO'
                                            group     = 'STANDARD'
                                            line      = 0
                                            for       = 'SDOKMET-DESCRIPT'
                                            rb_group  = ''
                                            label_for = ''
                                            label_ref = '' ) )
        where_used                 = VALUE ris_t_md_relationship(
                                          ( trobjtype   = 'ECTC'
                                            subtype     = ''
                                            legacy_type = 'GC'
                                            group       = 1 )
                                          ( trobjtype   = 'ECTC'
                                            subtype     = ''
                                            legacy_type = 'GW'
                                            group       = 1 ) )
        environment                = VALUE ris_t_md_relationship(
                                          ( trobjtype   = 'FUNC'
                                            subtype     = ''
                                            legacy_type = 'FF'
                                            group       = 1 )
                                          ( trobjtype   = 'DIAL'
                                            subtype     = ''
                                            legacy_type = 'A'
                                            group       = 1 ) )
        model_implementations      = VALUE ris_t_md_model_implementation( )
*      ( type_name              =
*        class_name             =
*        priority               =
*        default_implementation = ) )
        where_used_original_dynpro = VALUE ris_t_md_relationship(
                                          ( trobjtype   = 'ECAT'
                                            subtype     = ''
                                            legacy_type = 'GE'
                                            group       = 1 )
                                          ( trobjtype   = 'ECTC'
                                            subtype     = ''
                                            legacy_type = 'GW'
                                            group       = 1 ) )
) ).

    data(output) = table_view->prepare_output( i_table       = table
                                         i_table_title = 'TABLE' ).

  ENDMETHOD.


ENDCLASS.
