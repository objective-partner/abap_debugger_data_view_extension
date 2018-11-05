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
      itab_in_itab                FOR TESTING.
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




ENDCLASS.
