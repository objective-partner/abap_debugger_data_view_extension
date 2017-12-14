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
      display_tab_4_aunit FOR TESTING,
      check_4_correct_wrap FOR TESTING,
      check_4_negative_numbers FOR TESTING.
ENDCLASS.       "ltcl_aunit_debugger_table_enh


CLASS ltc_debug_data_view_table_enh IMPLEMENTATION.


  METHOD setup.
    CREATE OBJECT lo_aunit_debugger_table_enh.
  ENDMETHOD.


  METHOD teardown.
    CLEAR: lo_aunit_debugger_table_enh.
  ENDMETHOD.


  METHOD check_4_negative_numbers.
    TYPES: BEGIN OF negative_num_type,
             price TYPE s_price,
           END OF negative_num_type.

    DATA:
      lv_wrap_from_here TYPE                   i VALUE 255,
      ls_fieldcat       TYPE                   lvc_s_fcat,
      lt_fieldcatalog   TYPE                   lvc_t_fcat,
      lt_negative_num   TYPE STANDARD TABLE OF negative_num_type,
      lr_tabledescr     TYPE REF TO cl_abap_structdescr.

    lt_negative_num = VALUE #( ( price = -100 )
                               ( price = -99 )
                               ( price = -1 )
                               ( price = 0 )
                               ( price = 1 )
                               ( price = 99 )
                               ( price = 100 ) ).

    lr_tabledescr  ?= cl_abap_structdescr=>describe_by_data(  lt_negative_num[ 1 ] ).
*    lr_tabledescr->get_table_line_type( ).
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
      lv_wrap_from_here TYPE                   i VALUE 255,
      ls_fieldcat       TYPE                   lvc_s_fcat,
      lt_fieldcatalog   TYPE                   lvc_t_fcat,
      lr_tabdescr       TYPE REF TO            cl_abap_structdescr,
      lr_data           TYPE REF TO            data,
      lv_output         TYPE                   string,
      lt_outputlist     TYPE TABLE OF          string,
      lv_correct_length TYPE                   abap_bool VALUE abap_true,
      lv_output_length  TYPE                   i,
      lt_dfies          TYPE                   ddfields,
      ls_dfies          TYPE                   dfies,
      lt_sflight        TYPE STANDARD TABLE OF sflight.

    SELECT *
       FROM sflight
         INTO TABLE lt_sflight
           WHERE carrid = 'AA'.

    CREATE DATA lr_data LIKE LINE OF lt_sflight.

    lr_tabdescr ?= cl_abap_structdescr=>describe_by_data_ref( lr_data ).

    lt_dfies = cl_salv_data_descr=>read_structdescr( lr_tabdescr ).

    LOOP AT lt_dfies
    INTO    ls_dfies.

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
        lv_correct_length = abap_false.
      ENDIF.
    ENDLOOP.

    cl_abap_unit_assert=>assert_true(
      EXPORTING
        act              = lv_correct_length    " Actual value
        msg              = |Die Länge einer oder mehrerer Zeilen überschreitet die vorgegebene Länge.|    " Description
    ).

  ENDMETHOD.


  METHOD display_tab_4_aunit.

    TYPES: BEGIN OF ty_sflight_mini,
             carrid TYPE s_carr_id,
             connid TYPE s_conn_id,
             fldate TYPE s_date,
             price  TYPE s_price,
           END OF ty_sflight_mini.
    DATA: ls_fieldcat       TYPE                   lvc_s_fcat,
          it_fieldcatalog   TYPE                   lvc_t_fcat,
          lr_tabdescr       TYPE REF TO            cl_abap_structdescr,
          lr_data           TYPE REF TO            data,
          lt_dfies          TYPE                   ddfields,
          ls_dfies          TYPE                   dfies,
          lv_output         TYPE                   string,
          lt_outputlist     TYPE TABLE OF          string,
          lv_output_exp     TYPE                   string,
          lv_wrap_from_here TYPE                   i VALUE 255,
          lv_output_length  TYPE                   i,
          lv_correct_length TYPE                   abap_bool VALUE abap_true,
          lt_sflight        TYPE STANDARD TABLE OF ty_sflight_mini.

    SELECT carrid connid
           fldate price
       FROM sflight
         INTO TABLE lt_sflight
           WHERE carrid = 'AA'
           AND   connid = '0017'
           AND   fldate = '20170208'.


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

    lv_output_exp = |LT_SFLIGHT = VALUE #(\n  (    CARRID = 'AA'\t\n   CONNID = '0017'\t\n   FLDATE = '20170208'\t\n   PRICE = '422.94'\t )\n\n). |.

    cl_abap_unit_assert=>assert_equals(
      EXPORTING
        act                  = lv_output    " Data object with current value
        exp                  = lv_output_exp    " Data object with expected type
        msg                  = |String stimmt nicht überein.|
    ).


    SPLIT lv_output AT cl_abap_char_utilities=>newline INTO TABLE lt_outputlist.
    LOOP AT lt_outputlist INTO DATA(ls_output_elem).
      lv_output_length = strlen( ls_output_elem ).
      IF lv_output_length > lv_wrap_from_here.
        lv_correct_length = abap_false.
      ENDIF.
    ENDLOOP.

    cl_abap_unit_assert=>assert_true(
      EXPORTING
        act              = lv_correct_length    " Actual value
        msg              = |Die Länge einer oder mehrerer Zeilen überschreitet die vorgegebene Länge.|    " Description
    ).

  ENDMETHOD.

ENDCLASS.
