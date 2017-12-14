*&---------------------------------------------------------------------*
*& Report ZOP_AUNIT_EXT_DEMO
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT z_debugger_data_view_ext_demo.

DATA:
  lt_flights TYPE STANDARD TABLE OF sflight WITH EMPTY KEY,
  ls_flight  TYPE sflight.

"for testing table enhancement
SELECT * UP TO 5 ROWS
  FROM sflight
   INTO TABLE lt_flights
     WHERE carrid = 'AA'.

lt_flights[ 1 ]-price = -10.

cl_demo_output=>display( lt_flights ).

"for testing structure enhancement
SELECT SINGLE *
    FROM sflight
      INTO ls_flight.

cl_demo_output=>display( ls_flight ).
