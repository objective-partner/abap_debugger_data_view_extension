class ZCL_DEBUGGER_EHN_DEMO definition
  public
  final
  create public .

public section.
  PROTECTED SECTION.
  PRIVATE SECTION.

    METHODS do_someting_with_sflight_table
      IMPORTING
        !it_flights                         TYPE flighttab
      RETURNING
        VALUE(rv_logic_successfully_called) TYPE boole_d .
    METHODS do_someting_with_sflight_struc
      IMPORTING
        !is_flight                          TYPE sflight
      RETURNING
        VALUE(rv_logic_successfully_called) TYPE boole_d .
ENDCLASS.



CLASS ZCL_DEBUGGER_EHN_DEMO IMPLEMENTATION.


  METHOD do_someting_with_sflight_struc.
    rv_logic_successfully_called = abap_false.
    IF is_flight IS NOT INITIAL.
      "do some magic here
      rv_logic_successfully_called = abap_true.
    ENDIF.
  ENDMETHOD.


  METHOD do_someting_with_sflight_table.
    rv_logic_successfully_called = abap_false.
    IF it_flights[] IS NOT INITIAL.
      "do some magic here
      rv_logic_successfully_called = abap_true.
    ENDIF.
  ENDMETHOD.
ENDCLASS.
