*"* use this source file for your ABAP unit test classes
CLASS ltcl_pretty_printer_should DEFINITION DEFERRED.
CLASS zcl_op_value_pretty_printer DEFINITION LOCAL FRIENDS ltcl_pretty_printer_should.
CLASS ltcl_pretty_printer_should DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    DATA: cut TYPE REF TO zcl_op_value_pretty_printer.
    METHODS:
      setup,
      add_indent FOR TESTING RAISING cx_static_check,
      "! ABAP Editor does not support more than 255 lines.
      "! Issue: https://github.com/objective-partner/abap_debugger_data_view_extension/issues/9
      "! Real world example is an IDoc structure
      formatted_line_length_below255 FOR TESTING RAISING cx_static_check.
ENDCLASS.


CLASS ltcl_pretty_printer_should IMPLEMENTATION.

  METHOD setup.
    "as we are here in a private unit test class of zcl_op_value_pretty_printer we don't need to use the factory or injector for instantiating
    cut = NEW zcl_op_value_pretty_printer( ).
  ENDMETHOD.

  METHOD add_indent.

    "GIVEN
    DATA(expected) = |IS_NESTED_STRUCTURE = VALUE #(\r                              COL1 = '1'\r                              COL2 = VALUE #(\r              | &&
                     |                               COL1 = '1'\r                                             COL2 = '2'\r                                   | &&
                     |          )\r                              COL3 = '3'\r                              COL4 = VALUE #(\r                                 | &&
                     |            COL1 = '1'\r                                             COL2 = '2'\r                                             )\r      | &&
                     |                        ).|.

    DATA(input)    = |IS_NESTED_STRUCTURE = VALUE #( COL1 = '1' COL2 = VALUE #( COL1 = '1' COL2 = '2' ) COL3 = '3' COL4 = VALUE #( COL1 = '1' COL2 = '2' ) ).|.

    "WHEN
    DATA(formated_string) = cut->format( input ).

    "THEN
    cl_abap_unit_assert=>assert_equals(  EXPORTING  act  = formated_string
                                                    exp  = expected
                                                    msg  = |Indent was not added correct|       ).
  ENDMETHOD.

  METHOD formatted_line_length_below255.

    DATA lt_lines TYPE TABLE OF string.

    "GIVEN
    DATA(input) = |EDIDD = VALUE #( ( SEGNAM = 'E1EDK01' SDATA = 'EUREUR1.00000     0021             DE12345678901       DE12345678901       INVO1234561234| &&
                  |                         2.000             2.000             ABCDE                                                  1112223344          | &&
                  |     ABCDEDFGHIJKLMNOPQRSTUVWXYZ       AND STILL SOME MORE CHARACTERS - THERE WILL BE NO END                                            | &&
                  |                       - MAYBE THERE IS AN END IN SIGHT' ) ).|.

    "WHEN
    DATA(formated_string) = cut->format( input ).

    SPLIT formated_string AT zcl_op_value_pretty_printer=>c_newline INTO TABLE lt_lines.

    DATA(max_length) = 0.
    LOOP AT lt_lines INTO DATA(line).
      DATA(length) = strlen( line ).
      max_length = COND i( WHEN length >= max_length THEN length ELSE max_length ).
    ENDLOOP.

    "THEN
    cl_abap_unit_assert=>assert_number_between( lower = 0 upper = 255 number = max_length ).

  ENDMETHOD.


ENDCLASS.
