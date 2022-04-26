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
      handle_structure_in_itab           FOR TESTING,
      handle_itab_in_itab                FOR TESTING,
      st05_main_record_table      FOR TESTING RAISING cx_static_check,
      ris_t_metadata              FOR TESTING RAISING cx_static_check.
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

  METHOD handle_itab_in_itab.
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

    DATA:  output        TYPE string,
           output_exp    TYPE string,
           sflight_lines TYPE tt_flight_data_deep.


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


  METHOD handle_structure_in_itab.

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

  METHOD st05_main_record_table.

    DATA(lv_date) = sy-datum.
    DATA(lv_time) = sy-uzeit.

    DATA(table) = VALUE st05_main_record_table(
      ( date                           = lv_date
        time                           = lv_time
        duration                       = '10.5'
        number_of_rows                 = '11.5'
        object                         = 'OBJECT'
        statement_with_values          = `STATEMENT_WITH_VALUES`
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

    DATA(output) = table_view->prepare_output( i_table       = table
                                               i_table_title = 'TABLE' ).

    DATA(output_exp) = |TABLE = VALUE #( ( DATE = '{ lv_date }' TIME = '{ lv_time }' DURATION = '11 ' NUMBER_OF_ROWS = 12 |
                        && |OBJECT = 'OBJECT' STATEMENT_WITH_VALUES = `STATEMENT_WITH_VALUES` |
                        && |ARRAY_SIZE = 12 TRANSACTION = 'TRANSACTION' PROGRAM = 'PROGRAM' DB_CONNECTION_NAME = |
                        && |'DB_CONNECTION_NAME' DB_CONNECTION_ID = 'DB_CON_ID' OPERATION = 'OPERATI' RETURN_CODE |
                        && |= 13 WP_ID = '001' WP_TYPE = 'DIA' USER_NAME = 'USER_NAME' CLIENT = '100' TRANS_ID = |
                        && |'TRANS_ID' EPP_ROOT_ID = 'EPP_ROOT_ID' EPP_CONNECTION_ID = 'EPP_CONNECTION_ID' |
                        && |EPP_CONNECTION_COUNTER = 14 RECORD_NUMBER = 15 RECORD_NUMBERS = VALUE #( ( 16 ) |
                        && |( 17 ) ) OFFSET = 18 STATEMENT_WITH_NAMES = `STATEMENT_WITH_NAMES` LENGTH_OF_STATEMENT_WITH_NAMES |
                        && |= 19 VARIABLES = `VARIABLES` NUMBER_OF_VARIABLES = 20 TRACE_TYPE = 'TRAC' LINE_COLOR = 'LINE' ) ).|.
    cl_abap_unit_assert=>assert_equals( EXPORTING  act = output
                                                   exp = output_exp ).
  ENDMETHOD.


  METHOD ris_t_metadata.
    DATA(table) = VALUE ris_t_metadata(
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

) ).

    DATA(output) = table_view->prepare_output( i_table       = table
                                               i_table_title = 'TABLE' ).

    DATA(output_exp) = |TABLE = VALUE #( ( TROBJTYPE = '????' LEGACY_TYPE = 'DWY' SEARCH_GROUPS|
                        && | = VALUE #( ( NAME = 'STANDARD' TEXT_REF = `TEXT-S01` ) ( NAME = 'EINSTELLUNG' ) ) |
                        && |SEARCH_ELEMENTS = VALUE #( ( INDEX = 1 NAME = 'KEY1' TYPE = 'SO' GROUP = 'STANDARD' |
                        && |FOR = 'SDOKMEP-PROP_NAME' ) ( INDEX = 2 NAME = 'XTEXT' TYPE = 'SO' GROUP = 'STANDARD' |
                        && |FOR = 'SDOKMET-DESCRIPT' ) ) WHERE_USED = VALUE #( ( TROBJTYPE = 'ECTC' LEGACY_TYPE = 'GC' GROUP = 1 ) |
                        && |( TROBJTYPE = 'ECTC' LEGACY_TYPE = 'GW' GROUP = 1 ) ) ENVIRONMENT = VALUE #( ( TROBJTYPE = 'FUNC' LEGACY_TYPE = 'FF' GROUP = 1 ) |
                        && |( TROBJTYPE = 'DIAL' LEGACY_TYPE = 'A' GROUP = 1 ) ) ) ( TROBJTYPE = '????' LEGACY_TYPE = 'GP' |
                        && |SEARCH_GROUPS = VALUE #( ( NAME = 'STANDARD' TEXT_REF = `TEXT-S01` ) ( NAME = 'EINSTELLUNG' ) ) |
                        && |SEARCH_ELEMENTS = VALUE #( ( INDEX = 1 NAME = 'KEY1' TYPE = 'SO' GROUP = 'STANDARD' FOR = 'SDOKMEP-PROP_NAME' ) |
                        && |( INDEX = 2 NAME = 'XTEXT' TYPE = 'SO' GROUP = 'STANDARD' FOR = 'SDOKMET-DESCRIPT' ) ) |
                        && |WHERE_USED = VALUE #( ( TROBJTYPE = 'ECTC' LEGACY_TYPE = 'GC' GROUP = 1 ) |
                        && |( TROBJTYPE = 'ECTC' LEGACY_TYPE = 'GW' GROUP = 1 ) ) ENVIRONMENT = VALUE #( |
                        && |( TROBJTYPE = 'FUNC' LEGACY_TYPE = 'FF' GROUP = 1 ) ( TROBJTYPE = 'DIAL' LEGACY_TYPE = 'A' GROUP = 1 ) ) ) ).|.
    cl_abap_unit_assert=>assert_equals( EXPORTING  act = output
                                                   exp = output_exp ).

  ENDMETHOD.


ENDCLASS.
