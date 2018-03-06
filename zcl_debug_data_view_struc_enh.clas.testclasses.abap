CLASS ltc_debug_data_view_struc_enh DEFINITION DEFERRED.
CLASS zcl_debug_data_view_struc_enh DEFINITION LOCAL FRIENDS ltc_debug_data_view_struc_enh.

CLASS ltc_debug_data_view_struc_enh DEFINITION FOR TESTING DURATION SHORT  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    DATA:
    it_struc_data       TYPE tpda_struc_view_it.
    METHODS:
      process_simple_structure FOR TESTING,
      process_nested_structure FOR TESTING,
      process_mixed_structure FOR TESTING.
ENDCLASS.       "ltc_aunit_debugger_struc_enh


CLASS ltc_debug_data_view_struc_enh IMPLEMENTATION.

  METHOD process_simple_structure.

    DATA:
      iv_wrap_from_here   TYPE i,
      iv_struc_name       TYPE string,
      rv_content          TYPE string,
      rv_content_expected TYPE string.

    rv_content_expected = |IS_STRUCTURE = VALUE #(\tMANDT = '100'\tCARRID = 'AA'\tCONNID = '0017'\tFLDATE = '20141217'\tPRICE = |
                          && |'422.94'\tCURRENCY = 'USD'\tPLANETYPE = '747-400'\tSEATSMAX = '385'\t\rSEATSOCC = '372'\tPAYMENTSUM = '192437.84'\tSEATSMAX_B =|
                          && | '31'\tSEATSOCC_B = '28'\tSEATSMAX_F = '21'\tSEATSOCC_F = '21'\t).|.

    it_struc_data = VALUE #(
                            (    nr = '1'       component = 'MANDT'       varvalue = '100'       varvalhex = '310030003000'       vartype = 'C(3)'       varabstypename = '\TYPE=S_MANDT'       action1 = '@0Z\QFeldinhalt ändern@'
                             changable = 'X'       complong = 'LS_SFLIGHT-MANDT'       level = '1'       )
                            (    nr = '2'       component = 'CARRID'       varvalue = 'AA'       varvalhex = '410041002000'       vartype = 'C(3)'       varabstypename = '\TYPE=S_CARR_ID'       action1 = '@0Z\QFeldinhalt ändern@'
                             changable = 'X'       complong = 'LS_SFLIGHT-CARRID'       level = '1'       )
                            (    nr = '3'       component = 'CONNID'       varvalue = '0017'       varvalhex = '3000300031003700'       vartype = 'N(4)'       varabstypename = '\TYPE=S_CONN_ID'
                             action1 = '@0Z\QFeldinhalt ändern@'       changable = 'X'       complong = 'LS_SFLIGHT-CONNID'       level = '1'       )
                            (    nr = '4'       component = 'FLDATE'       varvalue = '20141217'       varvalhex = '32003000310034003100320031003700'       vartype = 'D(8)'       varabstypename = '\TYPE=S_DATE'
                             action1 = '@0Z\QFeldinhalt ändern@'       changable = 'X'       complong = 'LS_SFLIGHT-FLDATE'       level = '1'       )
                            (    nr = '5'       component = 'PRICE'       varvalue = '422.94 '       varvalhex = '000000000042294C'       vartype = 'P(8) DECIMALS 2 '       varabstypename = '\TYPE=S_PRICE'
                             action1 = '@0Z\QFeldinhalt ändern@'       changable = 'X'       complong = 'LS_SFLIGHT-PRICE'       level = '1'       )
                            (    nr = '6'       component = 'CURRENCY'       varvalue = 'USD'       varvalhex = '55005300440020002000'       vartype = 'C(5)'       varabstypename = '\TYPE=S_CURRCODE'
                             action1 = '@0Z\QFeldinhalt ändern@'       changable = 'X'       complong = 'LS_SFLIGHT-CURRENCY'       level = '1'       )
                            (    nr = '7'       component = 'PLANETYPE'       varvalue = '747-400'       varvalhex = '3700340037002D00340030003000200020002000'       vartype = 'C(10)'       varabstypename = '\TYPE=S_PLANETYE'
                             action1 = '@0Z\QFeldinhalt ändern@'       changable = 'X'       complong = 'LS_SFLIGHT-PLANETYPE'       level = '1'       )
                            (    nr = '8'       component = 'SEATSMAX'       varvalue = '385 '       varvalhex = '81010000'       vartype = 'I(4)'       varabstypename = '\TYPE=S_SEATSMAX'       action1 = '@0Z\QFeldinhalt ändern@'
                             changable = 'X'       complong = 'LS_SFLIGHT-SEATSMAX'       level = '1'       )
                            (    nr = '9'       component = 'SEATSOCC'       varvalue = '372 '       varvalhex = '74010000'       vartype = 'I(4)'       varabstypename = '\TYPE=S_SEATSOCC'       action1 = '@0Z\QFeldinhalt ändern@'
                             changable = 'X'       complong = 'LS_SFLIGHT-SEATSOCC'       level = '1'       )
                            (    nr = '10'       component = 'PAYMENTSUM'       varvalue = '192437.84 '       varvalhex = '00000000019243784C'       vartype = 'P(9) DECIMALS 2 '       varabstypename = '\TYPE=S_SUM'
                             action1 = '@0Z\QFeldinhalt ändern@'       changable = 'X'       complong = 'LS_SFLIGHT-PAYMENTSUM'       level = '1'       )
                            (    nr = '11'       component = 'SEATSMAX_B'       varvalue = '31 '       varvalhex = '1F000000'       vartype = 'I(4)'       varabstypename = '\TYPE=S_SMAX_B'       action1 = '@0Z\QFeldinhalt ändern@'
                             changable = 'X'       complong = 'LS_SFLIGHT-SEATSMAX_B'       level = '1'       )
                            (    nr = '12'       component = 'SEATSOCC_B'       varvalue = '28 '       varvalhex = '1C000000'       vartype = 'I(4)'       varabstypename = '\TYPE=S_SOCC_B'       action1 = '@0Z\QFeldinhalt ändern@'
                             changable = 'X'       complong = 'LS_SFLIGHT-SEATSOCC_B'       level = '1'       )
                            (    nr = '13'       component = 'SEATSMAX_F'       varvalue = '21 '       varvalhex = '15000000'       vartype = 'I(4)'       varabstypename = '\TYPE=S_SMAX_F'       action1 = '@0Z\QFeldinhalt ändern@'
                             changable = 'X'       complong = 'LS_SFLIGHT-SEATSMAX_F'       level = '1'       )
                            (    nr = '14'       component = 'SEATSOCC_F'       varvalue = '21 '       varvalhex = '15000000'       vartype = 'I(4)'       varabstypename = '\TYPE=S_SOCC_F'       action1 = '@0Z\QFeldinhalt ändern@'
                             changable = 'X'       complong = 'LS_SFLIGHT-SEATSOCC_F'       level = '1'       )
                          ).


    rv_content = NEW zcl_debug_data_view_struc_enh( )->prepare_output(
        it_struc_data = it_struc_data
        iv_wrap_from_here = 170
        iv_struc_name = |IS_STRUCTURE| ).


    cl_abap_unit_assert=>assert_equals(
      act   = rv_content
      exp   =  rv_content_expected
      msg   = 'Struktur String wurde nicht richtig aufbereitet'
    ).
  ENDMETHOD.

  METHOD process_nested_structure.

    it_struc_data = VALUE #(
                                ( nr = '         1' component = 'COL1'  varvalue = '1' varvalhex = '01000000' vartype = 'I(4)' varabstypename = '\TYPE=I'  changable = 'X' complong = 'NESTED_STRUC-COL1' level = '         1'   )
                                ( nr = '         2' component = 'COL2'  varvalue = 'Structure: flat, not charlike'  varvalhex = '0100000002000000'
                                                        vartype = 'Structure: flat, not charlike(8)' varabstypename = '\PROGRAM=Z_DEBUGGER_DATA_VIEW_EXT_DEMO\TYPE=T_COL2'
                                                                var_val_tech = 'X'  complong = 'NESTED_STRUC-COL2' level = '         1'   )
                                ( nr = '         3' component = '          COL1'  varvalue = '1' varvalhex = '01000000' vartype = 'I(4)' varabstypename = '\TYPE=I'  changable = 'X' complong = 'NESTED_STRUC-COL2-COL1' level = '         2'   )
                                ( nr = '         4' component = '          COL2'  varvalue = '2' varvalhex = '02000000' vartype = 'I(4)' varabstypename = '\TYPE=I'  changable = 'X' complong = 'NESTED_STRUC-COL2-COL2' level = '         2'   )

                           ).



    DATA(rv_content_expected) = |IS_NESTED_STRUCTURE = VALUE #(\tCOL1 = '1'\tCOL2 = VALUE #(\tCOL1 = '1'\tCOL2 = '2'\t)\t).|.


    DATA(rv_content) = NEW zcl_debug_data_view_struc_enh( )->prepare_output(
                                            it_struc_data = it_struc_data
                                            iv_wrap_from_here = 170
                                            iv_struc_name = |IS_NESTED_STRUCTURE| ).


    cl_abap_unit_assert=>assert_equals(
      act   = rv_content
      exp   =  rv_content_expected
      msg   = 'Nested structure was not formated as expected'
    ).

  ENDMETHOD.

  METHOD process_mixed_structure.
 it_struc_data = VALUE #(
                                ( nr = '         1' component = 'COL1'  varvalue = '1' varvalhex = '01000000' vartype = 'I(4)' varabstypename = '\TYPE=I'  changable = 'X' complong = 'NESTED_STRUC-COL1' level = '         1'   )
                                ( nr = '         2' component = 'COL2'  varvalue = 'Structure: flat, not charlike'  varvalhex = '0100000002000000'
                                                        vartype = 'Structure: flat, not charlike(8)' varabstypename = '\PROGRAM=Z_DEBUGGER_DATA_VIEW_EXT_DEMO\TYPE=T_COL2'
                                                                var_val_tech = 'X'  complong = 'NESTED_STRUC-COL2' level = '         1'   )
                                ( nr = '         3' component = '          COL1'  varvalue = '1' varvalhex = '01000000' vartype = 'I(4)' varabstypename = '\TYPE=I'  changable = 'X' complong = 'NESTED_STRUC-COL2-COL1' level = '         2'   )
                                ( nr = '         4' component = '          COL2'  varvalue = '2' varvalhex = '02000000' vartype = 'I(4)' varabstypename = '\TYPE=I'  changable = 'X' complong = 'NESTED_STRUC-COL2-COL2' level = '         2'   )
                                ( nr = '         5' component = 'COL3'  varvalue = '3' varvalhex = '03000000' vartype = 'I(4)' varabstypename = '\TYPE=I'  changable = 'X' complong = 'NESTED_STRUC-COL3' level = '         1'   )
                                ( nr = '         6' component = 'COL4'  varvalue = 'Structure: flat, not charlike'  varvalhex = '0100000002000000'
                                                        vartype = 'Structure: flat, not charlike(8)' varabstypename = '\PROGRAM=Z_DEBUGGER_DATA_VIEW_EXT_DEMO\TYPE=T_COL2'
                                                                var_val_tech = 'X'  complong = 'NESTED_STRUC-COL4' level = '         1'   )
                                ( nr = '         7' component = '          COL1'  varvalue = '1' varvalhex = '01000000' vartype = 'I(4)' varabstypename = '\TYPE=I'  changable = 'X' complong = 'NESTED_STRUC-COL4-COL1' level = '         2'   )
                                ( nr = '         8' component = '          COL2'  varvalue = '2' varvalhex = '02000000' vartype = 'I(4)' varabstypename = '\TYPE=I'  changable = 'X' complong = 'NESTED_STRUC-COL4-COL2' level = '         2'   )

                        ).



    DATA(rv_content_expected) = |IS_NESTED_STRUCTURE = VALUE #(\tCOL1 = '1'\tCOL2 = VALUE #(\tCOL1 = '1'\tCOL2 = '2'\t)\tCOL3 = '3'\tCOL4 = VALUE #(\tCOL1 = '1'\tCOL2 = '2'\t)\t).|.


    DATA(rv_content) = NEW zcl_debug_data_view_struc_enh( )->prepare_output(
                                            it_struc_data = it_struc_data
                                            iv_wrap_from_here = 170
                                            iv_struc_name = |IS_NESTED_STRUCTURE| ).


    cl_abap_unit_assert=>assert_equals(
      act   = rv_content
      exp   =  rv_content_expected
      msg   = 'Nested structure was not formated as expected'
    ).
  ENDMETHOD.

ENDCLASS.
