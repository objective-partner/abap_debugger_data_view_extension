CLASS ltc_struc_enh_should_process DEFINITION DEFERRED.
CLASS zcl_op_structure DEFINITION LOCAL FRIENDS ltc_struc_enh_should_process.

CLASS ltc_struc_enh_should_process DEFINITION FOR TESTING DURATION SHORT  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    METHODS:
      simple_structure FOR TESTING,
      nested_structure FOR TESTING,
      mixed_structure  FOR TESTING,
      itab_in_structure FOR TESTING,
      include_in_structure FOR TESTING.
ENDCLASS.       "ltc_aunit_debugger_struc_enh


CLASS ltc_struc_enh_should_process IMPLEMENTATION.

  METHOD simple_structure.

    DATA(content_expected) = |STRUCTURE = VALUE #( MANDT = '100' CARRID = 'AA' CONNID = '0017' FLDATE = '20141217' PRICE = '422.94' | &&
                             |CURRENCY = 'USD' PLANETYPE = '747-400' SEATSMAX = 385 SEATSOCC = 372 PAYMENTSUM = '192437.84' SEATSMAX_B | &&
                             |= 31 SEATSOCC_B = 28 SEATSMAX_F = 21 SEATSOCC_F = 21 ).|.
    TYPES: BEGIN OF test_type,
             mandt      TYPE s_mandt,
             carrid     TYPE s_carr_id,
             connid     TYPE s_conn_id,
             fldate     TYPE s_date,
             price      TYPE s_price,
             currency   TYPE s_currcode,
             planetype  TYPE s_planetye,
             seatsmax   TYPE s_seatsmax,
             seatsocc   TYPE s_seatsocc,
             paymentsum TYPE s_sum,
             seatsmax_b TYPE s_smax_b,
             seatsocc_b TYPE s_socc_b,
             seatsmax_f TYPE s_smax_f,
             seatsocc_f TYPE s_socc_f,
           END OF test_type.

    DATA(structure) = VALUE test_type(   mandt      =  '100'      carrid     =  'AA'
                                         connid     =  '0017'     fldate     =  '20141217'
                                         price      =  '422.94'   currency   =  'USD'
                                         planetype  =  '747-400'  seatsmax   =  385
                                         seatsocc   =  372        paymentsum =  '192437.84'
                                         seatsmax_b =  31         seatsocc_b =  28
                                         seatsmax_f =  21         seatsocc_f =  21  ).


    DATA(content) = NEW zcl_op_structure( )->prepare_output( i_structure          = structure
                                                             i_field_catalog      = NEW zcl_op_simple_field_catalog( )->get_by_data( i_structure = structure )
                                                             i_struc_name         = |STRUCTURE|  ).


    cl_abap_unit_assert=>assert_equals( act   = content
                                        exp   = content_expected
                                        msg   = 'Struktur String wurde nicht richtig aufbereitet'    ).
  ENDMETHOD.

  METHOD nested_structure.

    TYPES: BEGIN OF t_col2,
             col1 TYPE i,
             col2 TYPE i,
           END OF t_col2,
           BEGIN OF t_struct,
             col1 TYPE i,
             col2 TYPE t_col2,
           END OF t_struct,
           my_nested_struc_type TYPE t_struct.

    DATA(nested_structure) =  VALUE my_nested_struc_type( col1 = 1 col2 = VALUE #(
                                                                                    col1 = 1
                                                                                    col2 = 2
                                                                                   ) ).

    DATA(content_expected) = |NESTED_STRUCTURE = VALUE #( COL1 = 1 COL2 = VALUE #( COL1 = 1 COL2 = 2 ) ).|.


    DATA(content) = NEW zcl_op_structure( )->prepare_output( i_structure          = nested_structure
                                                             i_field_catalog      = NEW zcl_op_simple_field_catalog( )->get_by_data( i_structure = nested_structure )
                                                             i_struc_name         = |NESTED_STRUCTURE|  ).

    cl_abap_unit_assert=>assert_equals( act   = content
                                        exp   = content_expected
                                        msg   = 'Nested structure was not formated as expected'    ).

  ENDMETHOD.

  METHOD mixed_structure.
    TYPES: BEGIN OF t_col2,
             col1 TYPE i,
             col2 TYPE i,
           END OF t_col2,
           BEGIN OF t_struct,
             col1 TYPE i,
             col2 TYPE t_col2,
             col3 TYPE i,
             col4 TYPE t_col2,
           END OF t_struct,
           my_mixed_structure_type TYPE t_struct.

    DATA(mixed_structure) =  VALUE my_mixed_structure_type( col1 = 1
                                                             col2 = VALUE #(
                                                                             col1 = 1
                                                                             col2 = 2
                                                                            )
                                                             col3 = 3
                                                             col4 = VALUE #(
                                                                             col1 = 1
                                                                             col2 = 2
                                                                            )
                                                            ).

    DATA(content_expected) = |MIXED_STRUCTURE = VALUE #( COL1 = 1 COL2 = VALUE #( COL1 = 1 COL2 = 2 ) COL3 = 3 COL4 = VALUE #( COL1 = 1 COL2 = 2 ) ).|.

    DATA(content) = NEW zcl_op_structure( )->prepare_output(  i_structure          = mixed_structure
                                                              i_field_catalog      = NEW zcl_op_simple_field_catalog( )->get_by_data( i_structure = mixed_structure )
                                                              i_struc_name         = |MIXED_STRUCTURE|  ).

    cl_abap_unit_assert=>assert_equals( act   =  content
                                        exp   =  content_expected
                                        msg   = |Mixed structure was not formated as expected|    ).
  ENDMETHOD.


  METHOD itab_in_structure.

    TYPES: BEGIN OF t_col2,
             col1 TYPE i,
             col2 TYPE i,
           END OF t_col2,
           BEGIN OF t_struct,
             col1 TYPE i,
             col2 TYPE t_col2,
           END OF t_struct,
           my_itab_type TYPE STANDARD TABLE OF t_struct WITH DEFAULT KEY,
           BEGIN OF itab_in_struc_type,
             col1 TYPE i,
             col2 TYPE t_col2,
             col3 TYPE my_itab_type,
           END OF itab_in_struc_type.

    DATA(itab_in_struc) = VALUE itab_in_struc_type(   col1 = 1

                                                      col2 = VALUE #(
                                                                      col1 = 11
                                                                      col2 = 22
                                                                    )
                                                      col3 = VALUE #(
                                                                     ( col1 = 311 col2 = VALUE #( col1 = 3121 col2 = 3122 ) )
                                                                     ( col1 = 321 col2 = VALUE #( col1 = 3221 col2 = 3222 ) )
                                                                    )
                                                    ).


    DATA(content_expected) = |ITAB_IN_STRUCTURE = VALUE #( COL1 = 1 COL2 = VALUE #( COL1 = 11 COL2 = 22 ) COL3 = VALUE #(| &&
                             | ( COL1 = 311 COL2 = VALUE #( COL1 = 3121 COL2 = 3122 ) ) ( COL1 = 321 COL2 = VALUE #( COL1 = 3221 COL2 = 3222 ) ) ) ).|.

    DATA(content) = NEW zcl_op_structure( )->prepare_output(    i_structure          = itab_in_struc
                                                                i_field_catalog      = NEW zcl_op_simple_field_catalog( )->get_by_data( i_structure = itab_in_struc )
                                                                i_struc_name         = |ITAB_IN_STRUCTURE|  ).
    cl_abap_unit_assert=>assert_equals( act   =  content
                                        exp   =  content_expected
                                        msg   = |itab in structure was not formated as expected|    ).
  ENDMETHOD.


  METHOD include_in_structure.
    TYPES BEGIN OF t_struct.
            INCLUDE TYPE t000.
    TYPES dummy_field_1 TYPE i.
    TYPES dummy_field_2 TYPE string.
    TYPES END OF t_struct.

    DATA(include_in_structure) = VALUE t_struct( mandt = '100' mtext = 'someText' dummy_field_1 = 5 dummy_field_2 = 'someOtherText'  ).

    DATA(content_expected) = |INCLUDE_IN_STRUCTURE = VALUE #( MANDT = '100' MTEXT = 'someText' DUMMY_FIELD_1 = 5 DUMMY_FIELD_2 = `someOtherText` ).|.

    DATA(content) = NEW zcl_op_structure( )->prepare_output(    i_structure          = include_in_structure
                                                                i_field_catalog      = NEW zcl_op_simple_field_catalog( )->get_by_data( i_structure = include_in_structure )
                                                                i_struc_name         = |INCLUDE_IN_STRUCTURE|  ).
    cl_abap_unit_assert=>assert_equals( act   =  content
                                        exp   =  content_expected
                                        msg   = |include in structure was not formated as expected| ).


  ENDMETHOD.

ENDCLASS.

CLASS ltc_struc_enh_should_not DEFINITION DEFERRED.
CLASS zcl_op_structure DEFINITION LOCAL FRIENDS ltc_struc_enh_should_not.
CLASS ltc_struc_enh_should_not DEFINITION FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    METHODS: miss_value_operator FOR TESTING.

ENDCLASS.

CLASS ltc_struc_enh_should_not IMPLEMENTATION.

  METHOD miss_value_operator.

    TYPES:
      tab_type_controller_empty_itab TYPE STANDARD TABLE OF i WITH EMPTY KEY,
      BEGIN OF ts_base,
        col1 TYPE i,
        col2 TYPE i,
      END OF ts_base,
      BEGIN OF ts_shippment,
        controller      TYPE tab_type_controller_empty_itab,
        base            TYPE ts_base,
        client_order_id TYPE i,
        warehouse_id    TYPE i,
      END OF ts_shippment,
      tab_type_shippment TYPE STANDARD TABLE OF ts_shippment WITH EMPTY KEY,
      BEGIN OF ts_client_company,
        controller   TYPE tab_type_controller_empty_itab,
        base         TYPE ts_base,
        tt_shippment TYPE tab_type_shippment,
      END OF ts_client_company,
      tab_type_client_company TYPE STANDARD TABLE OF ts_client_company WITH EMPTY KEY,
      BEGIN OF ts_daily_processing,
        controller        TYPE tab_type_controller_empty_itab,
        base              TYPE ts_base,
        tt_client_company TYPE tab_type_client_company,
      END OF ts_daily_processing,
      BEGIN OF itab_in_struc_type,
        controller         TYPE tab_type_controller_empty_itab,
        s_daily_processing TYPE ts_daily_processing,
      END OF itab_in_struc_type.




    DATA(nested_structure) =  VALUE itab_in_struc_type( controller = VALUE #( )
                                                        s_daily_processing = VALUE #( controller = VALUE #( )
                                                                                      base = VALUE #( col1 = 1 col2 = 2 )
                                                                                      tt_client_company = VALUE #( ( controller = VALUE #( )
                                                                                                                base = VALUE #( col1 = 1 col2 = 2 )
                                                                                                                tt_shippment = VALUE #( ( controller = VALUE #( )
                                                                                                                                       base = VALUE #( col1 = 1 col2 = 2 )
                                                                                                                                       client_order_id = 1
                                                                                                                                       warehouse_id = 2    )
                                                                                                                                   )
                                                                                                               )

                                                                                                            )
                                                                                   )
                                                       ).

    DATA(content_expected) = |NESTED_STRUCTURE = VALUE #( S_DAILY_PROCESSING = VALUE #( BASE = VALUE #( COL1 = 1 COL2 = 2 ) | &&
                             |TT_CLIENT_COMPANY = VALUE #( ( BASE = VALUE #( COL1 = 1 COL2 = 2 ) TT_SHIPPMENT = VALUE #( ( BASE | &&
                             |= VALUE #( COL1 = 1 COL2 = 2 ) CLIENT_ORDER_ID = 1 WAREHOUSE_ID = 2 ) ) ) ) ) ).|.


    DATA(content) = NEW zcl_op_structure( )->prepare_output( i_structure          = nested_structure
                                                             i_field_catalog      = NEW zcl_op_simple_field_catalog( )->get_by_data( i_structure = nested_structure )
                                                             i_struc_name         = |NESTED_STRUCTURE|  ).

    cl_abap_unit_assert=>assert_equals( act   = content
                                        exp   = content_expected
                                        msg   = 'Nested structure was not formated as expected'    ).
  ENDMETHOD.

ENDCLASS.
