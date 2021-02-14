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
      table_of_integers FOR TESTING RAISING cx_static_check,
      table_in_structure FOR TESTING RAISING cx_static_check,
      performance FOR TESTING RAISING cx_static_check,
      string_literal FOR TESTING RAISING cx_static_check.
ENDCLASS.


CLASS ltcl_pretty_printer_should IMPLEMENTATION.

  METHOD setup.
    "as we are here in a private unit test class of zcl_op_value_pretty_printer we don't need to use the factory or injector for instantiating
    cut = NEW zcl_op_value_pretty_printer( ).
  ENDMETHOD.

  METHOD add_indent.

    "GIVEN
    DATA(input) = |IS_NESTED_STRUCTURE = VALUE #( COL1 = '1' COL2 = VALUE #( COL1 = '1' COL2 = '2' ) COL3 = '3' COL4 = VALUE #( COL1 = '1' COL2 = '2' ) ).|.

    "WHEN
    DATA(formated_string) = cut->format( input ).

    "THEN
    DATA(expected) = concat_lines_of(
        table = VALUE string_table(
            ( |IS_NESTED_STRUCTURE = VALUE #(| )
            ( |         COL1 = '1'| )
            ( |         COL2 = VALUE #(| )
            ( |                 COL1 = '1'| )
            ( |                 COL2 = '2'| )
            ( |                 )| )
            ( |         COL3 = '3'| )
            ( |         COL4 = VALUE #(| )
            ( |                 COL1 = '1'| )
            ( |                 COL2 = '2'| )
            ( |                 )| )
            ( |         ).| ) )
        sep = |\n| ).
    cl_abap_unit_assert=>assert_equals(  EXPORTING  act  = formated_string
                                                    exp  = expected
                                                    msg  = |Indent was not added correct|       ).
  ENDMETHOD.

  METHOD table_of_integers.

    "GIVEN
    DATA(input)    = |TABLE_OF_INTEGERS = VALUE #( ( 1 ) ( 2 ) ).|.

    "WHEN
    DATA(formated_string) = cut->format( input ).

    "THEN
    DATA(expected) = concat_lines_of(
        table = VALUE string_table(
            ( |TABLE_OF_INTEGERS = VALUE #(| )
            ( |         (| )
            ( |                 1 )| )
            ( |         (| )
            ( |                 2 )| )
            ( |         ).| ) )
        sep = |\n| ).
    cl_abap_unit_assert=>assert_equals(  EXPORTING  act  = formated_string
                                                    exp  = expected
                                                    msg  = |Indent was not added correct|       ).
  ENDMETHOD.

  METHOD table_in_structure.

    "GIVEN
    DATA(input)    = |TABLE_IN_STRUCTURE = VALUE #( TABLE = VALUE #( ( COL1 = '1' COL2 = '2' ) ( COL1 = '3' COL2 = '4' ) ) ).|.

    "WHEN
    DATA(formated_string) = cut->format( input ).

    "THEN
    DATA(expected) = concat_lines_of(
        table = VALUE string_table(
            ( |TABLE_IN_STRUCTURE = VALUE #(| )
            ( |         TABLE = VALUE #(| )
            ( |                 (| )
            ( |                         COL1 = '1'| )
            ( |                         COL2 = '2'| )
            ( |                         )| )
            ( |                 (| )
            ( |                         COL1 = '3'| )
            ( |                         COL2 = '4'| )
            ( |                         )| )
            ( |                 )| )
            ( |         ).| ) )
        sep = |\n| ).
    cl_abap_unit_assert=>assert_equals(  EXPORTING  act  = formated_string
                                                    exp  = expected
                                                    msg  = |Indent was not added correct|       ).

  ENDMETHOD.

  METHOD performance.

    "GIVEN
    DATA(input)    = |TABLE_IN_STRUCTURE = VALUE #(|
                  && repeat( val = | ( COL1 = '1' COL2 = '2' )| occ = 2000 )
                  && | ).|.

    "WHEN
    DATA(formated_string) = cut->format( input ).

    "THEN
    "method should take less than DURATION SHORT

  ENDMETHOD.

  METHOD string_literal.

    DATA lt_string TYPE TABLE OF string.
    DATA l_filename TYPE string.
    DATA input TYPE string.

    l_filename = '/tmp/sandra/abap_init_itab.txt'.

    OPEN DATASET l_filename FOR INPUT IN TEXT MODE ENCODING DEFAULT.
    READ DATASET l_filename INTO input.
    CLOSE DATASET l_filename.

    "WHEN
    DATA(formated_string) = cut->format( input ).

  ENDMETHOD.

ENDCLASS.
