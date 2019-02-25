*"* use this source file for your ABAP unit test classes
CLASS ltcl_pretty_printer_should DEFINITION DEFERRED.
CLASS zcl_op_value_pretty_printer_2 DEFINITION LOCAL FRIENDS ltcl_pretty_printer_should.
CLASS ltcl_pretty_printer_should DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    DATA cut TYPE REF TO zcl_op_value_pretty_printer_2.

    METHODS setup.
    "! test escaped apostrophes in the beginning, middle and end of string.
    METHODS apostrophes FOR TESTING RAISING cx_static_check.
    METHODS indent_and_line_breaks FOR TESTING RAISING cx_static_check.

    "! ABAP Editor does not support more than 255 lines.
    "! Issue: https://github.com/objective-partner/abap_debugger_data_view_extension/issues/9
    METHODS max_line_length_less_20 FOR TESTING RAISING cx_static_check.
    METHODS max_line_length_less_128 FOR TESTING RAISING cx_static_check.

ENDCLASS.

CLASS ltcl_pretty_printer_should IMPLEMENTATION.

  METHOD setup.
    "as we are here in a private unit test class so we don't need to use the factory or injector for instantiating
    cut = NEW #( indent_size = 2 max_line_length = 128 ).
  ENDMETHOD.


  METHOD apostrophes.
    "GIVEN
    cut = NEW #( indent_size = 2 max_line_length = 128 ).
    DATA(input) = |D = VALUE #( COL_1 = 'It''s easy (isn''t it?)' COL_2 = 'keep on doin''' COL_3 = '''ne Weile' COL_4 = '''' )|.
    DATA(expected) = |D = VALUE #(\r  COL_1 = 'It''s easy (isn''t it?)'\r  COL_2 = 'keep on doin'''\r  COL_3 = '''ne Weile'\r  COL_4 = ''''\r)|.

    "WHEN
    DATA(formatted_string) = cut->zif_op_value_pretty_printer~format( input ).

    "THEN
    cl_abap_unit_assert=>assert_equals(
      EXPORTING
        act = formatted_string
        exp = expected
        msg = |Apostrophes or brackets were not handled correctly.| ).
  ENDMETHOD.

  METHOD indent_and_line_breaks.
    "GIVEN
    cut = NEW #( indent_size = 2 max_line_length = 128 ).
    DATA(input) = |D = VALUE #( COL1 = '1' COL2 = VALUE #( COL1 = '1' COL2 = '2' ) COL3 = '3' COL4 = VALUE #( COL1 = '1' COL2 = '2' ) )|.
    DATA(expected) = |D = VALUE #(\r  COL1 = '1'\r  COL2 = VALUE #(\r    COL1 = '1'\r    COL2 = '2'\r  )\r  COL3 = '3'\r  COL4 = VALUE #(\r    COL1 = '1'\r    COL2 = '2'\r  )\r)|.

    "D = VALUE #(
    "  COL1 = '1'
    "  COL2 = VALUE #(
    "    COL1 = '1'
    "    COL2 = '2'
    "  )
    "  COL3 = '3'
    "  COL4 = VALUE #(
    "    COL1 = '1'
    "    COL2 = '2'
    "  )
    ").

    "WHEN
    DATA(formatted_string) = cut->zif_op_value_pretty_printer~format( input ).

    "THEN
    cl_abap_unit_assert=>assert_equals(
      EXPORTING
        act = formatted_string
        exp = expected
        msg = |Indent or line breaks were not added correctly| ).

  ENDMETHOD.

  METHOD max_line_length_less_20.
    "GIVEN
    cut = NEW #( indent_size = 2 max_line_length = 20 ).
    DATA(input) = |D = VALUE #( C1 = 'A123456789B123456789C123456789D123456789E123456789' )|.
    DATA(expected) = |D = VALUE #(\r  C1 = 'A123456789' &&\r  'B123456789C1234' &&\r  '56789D123456789' &&\r  'E123456789'\r)|.

    "WHEN
    DATA(formatted_string) = cut->zif_op_value_pretty_printer~format( input ).

    "THEN
    cl_abap_unit_assert=>assert_equals(
      EXPORTING
        act = formatted_string
        exp = expected
        msg = |Values exceeding maximum line length are not handled correctly.| ).

  ENDMETHOD.

  METHOD max_line_length_less_128.
    DATA lt_lines TYPE TABLE OF string.

    "GIVEN
    cut = NEW #( indent_size = 5 max_line_length = 128 ).
    DATA(input) = |EDIDD = VALUE #( ( SEGNAM = 'E1EDK01' SDATA = 'EUREUR1.00000     0021             DE12345678901       DE12345678901       INVO1234561234| &&
                  |                         2.000             2.000             ABCDE                                                  1112223344          | &&
                  |     ABCDEDFGHIJKLMNOPQRSTUVWXYZ       AND STILL SOME MORE CHARACTERS - THERE WILL BE NO END                                            | &&
                  |                       - MAYBE THERE IS AN END IN SIGHT' ) ).|.

    "WHEN
    DATA(formated_string) = cut->zif_op_value_pretty_printer~format( input ).

    SPLIT formated_string AT zcl_op_value_pretty_printer_2=>c_newline INTO TABLE lt_lines.
    DATA(max_length) = 0.
    LOOP AT lt_lines INTO DATA(line).
      DATA(length) = strlen( line ).
      max_length = COND i( WHEN length >= max_length THEN length ELSE max_length ).
    ENDLOOP.

    "THEN
    cl_abap_unit_assert=>assert_number_between( lower = 2 upper = 128 number = max_length ).

  ENDMETHOD.

ENDCLASS.
