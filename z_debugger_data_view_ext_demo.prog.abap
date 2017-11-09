*&---------------------------------------------------------------------*
*& Report ZOP_AUNIT_EXT_DEMO
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT z_debugger_data_view_ext_demo.

DATA:
      lt_sflight type STANDARD TABLE OF sflight,
      ls_sflight type sflight.

"for testing table enhancement
  SELECT *
    FROM sflight
     INTO TABLE lt_sflight
  WHERE carrid = 'AA'.

cl_demo_output=>display( lt_sflight ).

"for testing structure enhancement
SELECT SINGLE *
    from sflight
      into ls_sflight.

 cl_demo_output=>display( ls_sflight ).
