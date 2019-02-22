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

ENDCLASS.

CLASS ltcl_pretty_printer_should IMPLEMENTATION.

  METHOD setup.
    "as we are here in a private unit test class of zcl_op_value_pretty_printer we don't need to use the factory or injector for instantiating
    cut = NEW zcl_op_value_pretty_printer_2( indent_size = 2 max_line_length = 128 ).
  ENDMETHOD.


  METHOD apostrophes.
    "GIVEN
    DATA(input) = |D = VALUE #( COL_1 = 'It''s easy (isn''t it?)' COL_2 = 'keep on doin''' COL_3 = '''ne Weile' COL_4 = '''' ).|.

    DATA(expected) = |D = VALUE #(\r  COL_1 = 'It''s easy (isn''t it?)'\r  COL_2 = 'keep on doin'''\r  COL_3 = '''ne Weile'\r  COL_4 = ''''\r).|.

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
    DATA(input) = |D = VALUE #( COL1 = '1' COL2 = VALUE #( COL1 = '1' COL2 = '2' ) COL3 = '3' COL4 = VALUE #( COL1 = '1' COL2 = '2' ) ).|.

    DATA(expected) = |D = VALUE #(\r  COL1 = '1'\r  COL2 = VALUE #(\r    COL1 = '1'\r    COL2 = '2'\r  )\r  COL3 = '3'\r  COL4 = VALUE #(\r    COL1 = '1'\r  COL2 = '2'\r  )\r)|.

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

ENDCLASS.
