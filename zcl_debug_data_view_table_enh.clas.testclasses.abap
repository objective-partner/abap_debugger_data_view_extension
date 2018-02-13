CLASS ltc_debug_data_view_table_enh DEFINITION DEFERRED.
CLASS zcl_debug_data_view_table_enh   DEFINITION LOCAL FRIENDS ltc_debug_data_view_table_enh.

CLASS ltc_debug_data_view_table_enh DEFINITION FOR TESTING
  DURATION SHORT  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    DATA:
      lo_aunit_debugger_table_enh TYPE REF TO zcl_debug_data_view_table_enh.  "class under test

    METHODS: setup.
    METHODS: teardown.
    METHODS:
      "!check of
      prepare_ouput_w_1_line_itab FOR TESTING,
      check_4_correct_wrap FOR TESTING,
      "!check for
      correct_negative_numbers_value FOR TESTING.
ENDCLASS.       "ltcl_aunit_debugger_table_enh


CLASS ltc_debug_data_view_table_enh IMPLEMENTATION.


  METHOD setup.
    lo_aunit_debugger_table_enh = NEW zcl_debug_data_view_table_enh( ).
  ENDMETHOD.


  METHOD teardown.
    CLEAR lo_aunit_debugger_table_enh.
  ENDMETHOD.


  METHOD correct_negative_numbers_value.
    TYPES: BEGIN OF negative_num_type,
             price TYPE s_price,
           END OF   negative_num_type.

    DATA:
      lv_wrap_from_here TYPE                   i VALUE 255,
      ls_fieldcat       TYPE                   lvc_s_fcat,
      lt_fieldcatalog   TYPE                   lvc_t_fcat,
      lt_negative_num   TYPE STANDARD TABLE OF negative_num_type WITH EMPTY KEY,
      lr_tabledescr     TYPE REF TO cl_abap_structdescr.

    lt_negative_num = VALUE #( ( price = -100 )
                               ( price = -99 )
                               ( price = -1 )
                               ( price = 0 )
                               ( price = 1 )
                               ( price = 99 )
                               ( price = 100 ) ).

    lr_tabledescr  ?= cl_abap_structdescr=>describe_by_data(  lt_negative_num[ 1 ] ).

    DATA(lt_dfies) = cl_salv_data_descr=>read_structdescr(  lr_tabledescr  ).

    LOOP AT lt_dfies  INTO DATA(ls_dfies).
      CLEAR ls_fieldcat.
      MOVE-CORRESPONDING ls_dfies TO ls_fieldcat.
      ls_fieldcat-seltext = ls_dfies-fieldname.
      APPEND ls_fieldcat TO lt_fieldcatalog.
    ENDLOOP.

    lv_wrap_from_here = 170.

    DATA(lv_output) = lo_aunit_debugger_table_enh->prepare_output(
                        it_table          = lt_negative_num
                        iv_table_title    = 'LT_NUMBERS'
                        it_fieldcatalog   = lt_fieldcatalog
                        iv_wrap_from_here = lv_wrap_from_here ).

  ENDMETHOD.
  METHOD check_4_correct_wrap.
    DATA:
      lv_wrap_from_here            TYPE                   i VALUE 255,
      ls_fieldcat                  TYPE                   lvc_s_fcat,
      lt_fieldcatalog              TYPE                   lvc_t_fcat,
      lr_tabdescr                  TYPE REF TO            cl_abap_structdescr,
      lr_data                      TYPE REF TO            data,
      lv_output                    TYPE                   string,
      lt_outputlist                TYPE TABLE OF          string,
      lv_predifined_wrap_respected TYPE                   abap_bool VALUE abap_true,
      lv_output_length             TYPE                   i,
      lt_dfies                     TYPE                   ddfields,
      ls_dfies                     TYPE                   dfies,
      lt_sflight                   TYPE STANDARD TABLE OF sflight.


    lt_sflight = VALUE #(
      (    mandt = '001'     carrid = 'AA'   connid = '0017'     fldate = '20170810'     price = '422.94 '   currency = 'USD'    planetype = '747-400'   seatsmax = '385 '   seatsocc = '373 '
       paymentsum = '191448.29 '     seatsmax_b = '31 '  seatsocc_b = '31 '  seatsmax_f = '21 '  seatsocc_f = '19 '  )
      (    mandt = '001'     carrid = 'AA'   connid = '0017'     fldate = '20170911'     price = '422.94 '   currency = 'USD'    planetype = '747-400'   seatsmax = '385 '   seatsocc = '372 '
       paymentsum = '192949.65 '     seatsmax_b = '31 '  seatsocc_b = '30 '  seatsmax_f = '21 '  seatsocc_f = '20 '  )
      (    mandt = '001'     carrid = 'AA'   connid = '0017'     fldate = '20171013'     price = '422.94 '   currency = 'USD'    planetype = '747-400'   seatsmax = '385 '   seatsocc = '374 '
       paymentsum = '193761.65 '     seatsmax_b = '31 '  seatsocc_b = '29 '  seatsmax_f = '21 '  seatsocc_f = '21 '  )
      (    mandt = '001'     carrid = 'AA'   connid = '0017'     fldate = '20171114'     price = '422.94 '   currency = 'USD'    planetype = '747-400'   seatsmax = '385 '   seatsocc = '369 '
       paymentsum = '191634.32 '     seatsmax_b = '31 '  seatsocc_b = '30 '  seatsmax_f = '21 '  seatsocc_f = '20 '  )
      (    mandt = '001'     carrid = 'AA'   connid = '0017'     fldate = '20171216'     price = '422.94 '   currency = 'USD'    planetype = '747-400'   seatsmax = '385 '   seatsocc = '373 '
       paymentsum = '194387.65 '     seatsmax_b = '31 '  seatsocc_b = '31 '  seatsmax_f = '21 '  seatsocc_f = '21 '  )
      (    mandt = '001'     carrid = 'AA'   connid = '0017'     fldate = '20180117'     price = '422.94 '   currency = 'USD'    planetype = '747-400'   seatsmax = '385 '   seatsocc = '373 '
       paymentsum = '192471.82 '     seatsmax_b = '31 '  seatsocc_b = '30 '  seatsmax_f = '21 '  seatsocc_f = '19 '  )
      (    mandt = '001'     carrid = 'AA'   connid = '0017'     fldate = '20180218'     price = '422.94 '   currency = 'USD'    planetype = '747-400'   seatsmax = '385 '   seatsocc = '364 '
       paymentsum = '189764.88 '     seatsmax_b = '31 '  seatsocc_b = '30 '  seatsmax_f = '21 '  seatsocc_f = '20 '  )
      (    mandt = '001'     carrid = 'AA'   connid = '0017'     fldate = '20180322'     price = '422.94 '   currency = 'USD'    planetype = '747-400'   seatsmax = '385 '   seatsocc = '371 '
       paymentsum = '191854.22 '     seatsmax_b = '31 '  seatsocc_b = '29 '  seatsmax_f = '21 '  seatsocc_f = '20 '  )
      (    mandt = '001'     carrid = 'AA'   connid = '0017'     fldate = '20180423'     price = '422.94 '   currency = 'USD'    planetype = '747-400'   seatsmax = '385 '   seatsocc = '67 '
       paymentsum = '35696.14 '  seatsmax_b = '31 '  seatsocc_b = '6 '   seatsmax_f = '21 '  seatsocc_f = '4 '   )
      (    mandt = '001'     carrid = 'AA'   connid = '0017'     fldate = '20180525'     price = '422.94 '   currency = 'USD'    planetype = '747-400'   seatsmax = '385 '   seatsocc = '161 '
       paymentsum = '82701.75 '  seatsmax_b = '31 '  seatsocc_b = '12 '  seatsmax_f = '21 '  seatsocc_f = '9 '   )
      (    mandt = '001'     carrid = 'AA'   connid = '0017'     fldate = '20180626'     price = '422.94 '   currency = 'USD'    planetype = '747-400'   seatsmax = '385 '   seatsocc = '55 '
       paymentsum = '29068.70 '  seatsmax_b = '31 '  seatsocc_b = '5 '   seatsmax_f = '21 '  seatsocc_f = '3 '   )
      (    mandt = '001'     carrid = 'AA'   connid = '0017'     fldate = '20180728'     price = '422.94 '   currency = 'USD'    planetype = '747-400'   seatsmax = '385 '   seatsocc = '81 '
       paymentsum = '42446.28 '  seatsmax_b = '31 '  seatsocc_b = '6 '   seatsmax_f = '21 '  seatsocc_f = '5 '   )
      (    mandt = '001'     carrid = 'AA'   connid = '0017'     fldate = '20180829'     price = '422.94 '   currency = 'USD'    planetype = '747-400'   seatsmax = '385 '   seatsocc = '26 '
       paymentsum = '11482.83 '  seatsmax_b = '31 '  seatsocc_b = '2 '   seatsmax_f = '21 '  )
      (    mandt = '001'     carrid = 'AA'   connid = '0064'     fldate = '20170812'     price = '422.94 '   currency = 'USD'    planetype = 'A340-600'  seatsmax = '330 '   seatsocc = '316 '
       paymentsum = '169307.25 '     seatsmax_b = '30 '  seatsocc_b = '30 '  seatsmax_f = '20 '  seatsocc_f = '18 '  )
      (    mandt = '001'     carrid = 'AA'   connid = '0064'     fldate = '20170913'     price = '422.94 '   currency = 'USD'    planetype = 'A340-600'  seatsmax = '330 '   seatsocc = '319 '
       paymentsum = '170990.57 '     seatsmax_b = '30 '  seatsocc_b = '30 '  seatsmax_f = '20 '  seatsocc_f = '19 '  )
      (    mandt = '001'     carrid = 'AA'   connid = '0064'     fldate = '20171015'     price = '422.94 '   currency = 'USD'    planetype = 'A340-600'  seatsmax = '330 '   seatsocc = '312 '
       paymentsum = '167898.89 '     seatsmax_b = '30 '  seatsocc_b = '28 '  seatsmax_f = '20 '  seatsocc_f = '20 '  )
      (    mandt = '001'     carrid = 'AA'   connid = '0064'     fldate = '20171116'     price = '422.94 '   currency = 'USD'    planetype = 'A340-600'  seatsmax = '330 '   seatsocc = '321 '
       paymentsum = '170888.98 '     seatsmax_b = '30 '  seatsocc_b = '29 '  seatsmax_f = '20 '  seatsocc_f = '19 '  )
      (    mandt = '001'     carrid = 'AA'   connid = '0064'     fldate = '20171218'     price = '422.94 '   currency = 'USD'    planetype = 'A340-600'  seatsmax = '330 '   seatsocc = '319 '
       paymentsum = '171003.34 '     seatsmax_b = '30 '  seatsocc_b = '30 '  seatsmax_f = '20 '  seatsocc_f = '19 '  )
      (    mandt = '001'     carrid = 'AA'   connid = '0064'     fldate = '20180119'     price = '422.94 '   currency = 'USD'    planetype = 'A340-600'  seatsmax = '330 '   seatsocc = '311 '
       paymentsum = '165691.17 '     seatsmax_b = '30 '  seatsocc_b = '28 '  seatsmax_f = '20 '  seatsocc_f = '19 '  )
      (    mandt = '001'     carrid = 'AA'   connid = '0064'     fldate = '20180220'     price = '422.94 '   currency = 'USD'    planetype = 'A340-600'  seatsmax = '330 '   seatsocc = '321 '
       paymentsum = '171967.61 '     seatsmax_b = '30 '  seatsocc_b = '28 '  seatsmax_f = '20 '  seatsocc_f = '20 '  )
      (    mandt = '001'     carrid = 'AA'   connid = '0064'     fldate = '20180324'     price = '422.94 '   currency = 'USD'    planetype = 'A340-600'  seatsmax = '330 '   seatsocc = '319 '
       paymentsum = '171324.64 '     seatsmax_b = '30 '  seatsocc_b = '29 '  seatsmax_f = '20 '  seatsocc_f = '20 '  )
      (    mandt = '001'     carrid = 'AA'   connid = '0064'     fldate = '20180425'     price = '422.94 '   currency = 'USD'    planetype = 'A340-600'  seatsmax = '330 '   seatsocc = '148 '
       paymentsum = '78878.40 '  seatsmax_b = '30 '  seatsocc_b = '13 '  seatsmax_f = '20 '  seatsocc_f = '9 '   )
      (    mandt = '001'     carrid = 'AA'   connid = '0064'     fldate = '20180527'     price = '422.94 '   currency = 'USD'    planetype = 'A340-600'  seatsmax = '330 '   seatsocc = '109 '
       paymentsum = '58061.32 '  seatsmax_b = '30 '  seatsocc_b = '10 '  seatsmax_f = '20 '  seatsocc_f = '7 '   )
      (    mandt = '001'     carrid = 'AA'   connid = '0064'     fldate = '20180628'     price = '422.94 '   currency = 'USD'    planetype = 'A340-600'  seatsmax = '330 '   seatsocc = '116 '
       paymentsum = '61317.93 '  seatsmax_b = '30 '  seatsocc_b = '11 '  seatsmax_f = '20 '  seatsocc_f = '6 '   )
      (    mandt = '001'     carrid = 'AA'   connid = '0064'     fldate = '20180730'     price = '422.94 '   currency = 'USD'    planetype = 'A340-600'  seatsmax = '330 '   seatsocc = '8 '
       paymentsum = '3848.76 '   seatsmax_b = '30 '  seatsocc_b = '1 '   seatsmax_f = '20 '  )
      (    mandt = '001'     carrid = 'AA'   connid = '0064'     fldate = '20180831'     price = '422.94 '   currency = 'USD'    planetype = 'A340-600'  seatsmax = '330 '   seatsmax_b = '30 '
       seatsmax_f = '20 '    )

    ).



    CREATE DATA lr_data LIKE LINE OF lt_sflight.

    lr_tabdescr ?= cl_abap_structdescr=>describe_by_data_ref( lr_data ).

    lt_dfies = cl_salv_data_descr=>read_structdescr( lr_tabdescr ).

    LOOP AT lt_dfies  INTO    ls_dfies.

      CLEAR ls_fieldcat.

      MOVE-CORRESPONDING ls_dfies TO ls_fieldcat.
      ls_fieldcat-seltext = ls_dfies-fieldname.
      APPEND ls_fieldcat TO lt_fieldcatalog.

    ENDLOOP.

    lv_wrap_from_here = 170.

    lv_output = lo_aunit_debugger_table_enh->prepare_output(
                        it_table          = lt_sflight
                        iv_table_title    = 'LT_SFLIGHT'
                        it_fieldcatalog   = lt_fieldcatalog
                        iv_wrap_from_here = lv_wrap_from_here ).


    SPLIT lv_output AT cl_abap_char_utilities=>newline INTO TABLE lt_outputlist.
    LOOP AT lt_outputlist INTO DATA(ls_output_elem).
      lv_output_length = strlen( ls_output_elem ).
      IF lv_output_length > lv_wrap_from_here.
        lv_predifined_wrap_respected = abap_false.
        cl_abap_unit_assert=>fail( EXPORTING msg = |Wrap from value of { lv_wrap_from_here  } ignored with length { lv_output_length } at index { sy-index } |
                                             detail = |{ ls_output_elem }| ).
      ENDIF.
    ENDLOOP.

  ENDMETHOD.


  METHOD prepare_ouput_w_1_line_itab.

    TYPES: BEGIN OF ty_sflight_mini,
             carrid TYPE s_carr_id,
             connid TYPE s_conn_id,
             fldate TYPE s_date,
             price  TYPE s_price,
           END OF ty_sflight_mini.
    DATA: ls_fieldcat                  TYPE                   lvc_s_fcat,
          it_fieldcatalog              TYPE                   lvc_t_fcat,
          lr_tabdescr                  TYPE REF TO            cl_abap_structdescr,
          lr_data                      TYPE REF TO            data,
          lt_dfies                     TYPE                   ddfields,
          ls_dfies                     TYPE                   dfies,
          lv_output                    TYPE                   string,
          lt_outputlist                TYPE TABLE OF          string,
          lv_output_exp                TYPE                   string,
          lv_wrap_from_here            TYPE                   i VALUE 255,
          lv_output_length             TYPE                   i,
          lv_predifined_wrap_respected TYPE                   abap_bool VALUE abap_true,
          lt_sflight                   TYPE STANDARD TABLE OF ty_sflight_mini.


    lt_sflight = VALUE #(  ( carrid = 'AA'   connid = '0017' fldate = '20170810' price = '422.94 ' ) ).

    CREATE DATA lr_data LIKE LINE OF lt_sflight.

    lr_tabdescr ?= cl_abap_structdescr=>describe_by_data_ref( lr_data ).

    lt_dfies = cl_salv_data_descr=>read_structdescr( lr_tabdescr ).

    LOOP AT lt_dfies
    INTO    ls_dfies.

      CLEAR ls_fieldcat.

      MOVE-CORRESPONDING ls_dfies TO ls_fieldcat.
      ls_fieldcat-seltext = ls_dfies-fieldname.
      APPEND ls_fieldcat TO it_fieldcatalog.

    ENDLOOP.

    lv_wrap_from_here = 30.

    lv_output = lo_aunit_debugger_table_enh->prepare_output(
        it_table = lt_sflight
        iv_table_title = 'LT_SFLIGHT'
        it_fieldcatalog = it_fieldcatalog
        iv_wrap_from_here  = lv_wrap_from_here ).

    lv_output_exp = |LT_SFLIGHT = VALUE #(\n  (    CARRID = 'AA'\t\n   CONNID = '0017'\t\n   FLDATE = '20170810'\t\n   PRICE = '422.94'\t )\n\n). |.

    "#TODO: replace this comparison with one that exactly shows chars wich are different to expectation
    cl_abap_unit_assert=>assert_equals(
      EXPORTING
        act                  = lv_output    " Data object with current value
        exp                  = lv_output_exp    " Data object with expected type
        msg                  = |Character String Different|

    ).

  ENDMETHOD.

ENDCLASS.
