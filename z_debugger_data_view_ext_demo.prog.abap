*&---------------------------------------------------------------------*
*& Report z_debugger_data_view_ext_demo
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT z_debugger_data_view_ext_demo.

DATA:
  lt_flights TYPE ty_flights,
  ls_flight  TYPE sflight.

"for testing table enhancement
SELECT * UP TO 5 ROWS
  FROM sflight
   INTO TABLE lt_flights
     WHERE carrid = 'AA'.

"for testing structure enhancement
SELECT SINGLE *
    FROM sflight
      INTO ls_flight.

"nested examples
TYPES: BEGIN OF t_col2,
         col1 TYPE i,
         col2 TYPE i,
       END OF t_col2.

TYPES: BEGIN OF t_struct,
         col1 TYPE i,
         col2 TYPE t_col2,
       END OF t_struct.


TYPES: my_itab_type         TYPE STANDARD TABLE OF t_struct WITH DEFAULT KEY,
       my_nested_struc_type TYPE t_struct.


TYPES: BEGIN OF nested_type,
         col1 TYPE i,
         col2 TYPE t_col2,
         col3 TYPE my_itab_type,
       END OF nested_type.


TYPES: my_nested_itab_type TYPE STANDARD TABLE OF nested_type WITH DEFAULT KEY.

DATA: nested_itab   TYPE my_nested_itab_type,
      nested_struc  TYPE my_nested_struc_type,
      itab_in_struc TYPE nested_type.


"build structures
nested_struc =     VALUE my_nested_struc_type( col1 = 1 col2 = VALUE #(
                                                                        col1 = 1
                                                                        col2 = 2
                                                                       )
                                              ).
itab_in_struc = VALUE nested_type( col1 = 1 col2 = VALUE #(
                                                             col1 = 1
                                                             col2 = 2
                                                           )
                                            col3 = VALUE #(
                                                            ( col1 = 1 col2 = VALUE #( col1 = 1 col2 = 2 ) )
                                                            ( col1 = 1 col2 = VALUE #( col1 = 1 col2 = 2 ) )
                                                           )
                                  ).


"build itab
nested_itab  =     VALUE my_nested_itab_type( (   col1 = 1  col2 = VALUE #( col1 = 1 col2 = 2 )
                                                            col3 = VALUE #( "start here a nested itab
                                                                            ( col1 = 1 )  "first row of it
                                                                            ( col2-col2 = 2 )
                                                                           )
                                               )
                                             ).



"do something with structure
DATA(ls_col2) = nested_struc-col2.


"do something with itab
DATA(ls_col1) = nested_itab[ 1 ]-col1.
