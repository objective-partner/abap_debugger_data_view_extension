*"* use this source file for your ABAP unit test classes
CLASS ltc_debugger_ehn_demo DEFINITION DEFERRED.
CLASS zcl_debugger_ehn_demo DEFINITION LOCAL FRIENDS ltc_debugger_ehn_demo.

CLASS ltc_debugger_ehn_demo DEFINITION FOR TESTING
  DURATION SHORT  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    DATA:
      lo_cut TYPE REF TO zcl_debugger_ehn_demo.  "class under test

    METHODS:
      setup,
      structure_call_must_be_ok FOR TESTING,
      table_call_must_be_ok FOR TESTING.
ENDCLASS.       "ltc_Debugger_Ehn_Demo


CLASS ltc_debugger_ehn_demo IMPLEMENTATION.
  METHOD setup.
    lo_cut = NEW zcl_debugger_ehn_demo( ).
  ENDMETHOD.

  METHOD table_call_must_be_ok.

    DATA lt_flights TYPE flighttab.
    DATA rv_logic_successfully_called TYPE boole_d.

    "TODO: need to provide it_flights test data here
    lt_flights = VALUE #(
      (    mandt = '001'       carrid = 'AA'       connid = '0017'       fldate = '20170208'       price = '422.94'       currency = 'USD'       planetype = '747‑400'       seatsmax = '385'       seatsocc = '374'       paymentsum = '193021.62'
    seatsmax_b = '31'       seatsocc_b = '31'
       seatsmax_f = '21'       seatsocc_f = '19'       )
      (    mandt = '001'       carrid = 'AA'       connid = '0017'       fldate = '20170308'       price = '422.94'       currency = 'USD'       planetype = '747‑400'       seatsmax = '385'       seatsocc = '364'       paymentsum = '188813.32'
    seatsmax_b = '31'       seatsocc_b = '29'
       seatsmax_f = '21'       seatsocc_f = '20'       )
      (    mandt = '001'       carrid = 'AA'       connid = '0017'       fldate = '20170405'       price = '422.94'       currency = 'USD'       planetype = '747‑400'       seatsmax = '385'       seatsocc = '374'       paymentsum = '194645.69'
    seatsmax_b = '31'       seatsocc_b = '29'
       seatsmax_f = '21'       seatsocc_f = '21'       )
      (    mandt = '001'       carrid = 'AA'       connid = '0017'       fldate = '20170503'       price = '422.94'       currency = 'USD'       planetype = '747‑400'       seatsmax = '385'       seatsocc = '371'       paymentsum = '193224.54'
    seatsmax_b = '31'       seatsocc_b = '30'
       seatsmax_f = '21'       seatsocc_f = '20'       )
      (    mandt = '001'       carrid = 'AA'       connid = '0017'       fldate = '20170531'       price = '422.94'       currency = 'USD'       planetype = '747‑400'       seatsmax = '385'       seatsocc = '370'       paymentsum = '193791.27'
    seatsmax_b = '31'       seatsocc_b = '31'
       seatsmax_f = '21'       seatsocc_f = '21'       )

    ).
    rv_logic_successfully_called =  lo_cut->do_someting_with_sflight_table( EXPORTING  it_flights = lt_flights ).

    cl_abap_unit_assert=>assert_true(
      EXPORTING
        act              = rv_logic_successfully_called    " Actual value
        msg              = |table call was not succsessfull|
    ).
  ENDMETHOD.

  METHOD structure_call_must_be_ok.

    DATA ls_flight TYPE sflight.
    DATA rv_logic_successfully_called TYPE boole_d.

    "TODO: need to provide is_flight test data here
    ls_flight = VALUE #(    mandt = '001'         carrid = 'AA'         connid = '0017'         fldate = '20170208'         price = '422.94 '         currency = 'USD'         planetype = '747‑400'         seatsmax = '385 '         seatsocc = '374 '
   paymentsum = '193021.62 '         seatsmax_b = '31 '         seatsocc_b = '31 '         seatsmax_f = '21 '         seatsocc_f = '19 '       ).

    rv_logic_successfully_called =  lo_cut->do_someting_with_sflight_struc( EXPORTING is_flight = ls_flight ).

    cl_abap_unit_assert=>assert_true(
      EXPORTING
        act              = rv_logic_successfully_called    " Actual value
        msg              = |structure call was not succsessfull|
    ).
  ENDMETHOD.




ENDCLASS.
