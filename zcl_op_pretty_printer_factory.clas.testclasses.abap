*"* use this source file for your ABAP unit test classes
CLASS ltcl_pp_factory_should DEFINITION DEFERRED.
CLASS zcl_op_pretty_printer_factory DEFINITION LOCAL FRIENDS ltcl_pp_factory_should.
CLASS ltcl_pp_factory_should DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    METHODS:
      find_standard_pretty_printer FOR TESTING RAISING cx_static_check,
      create_standard_pretty_printer FOR TESTING RAISING cx_static_check.
ENDCLASS.


CLASS ltcl_pp_factory_should IMPLEMENTATION.


  METHOD find_standard_pretty_printer.
    "GIVEN - ...

    "WHEN
    DATA(classes) = zcl_op_pretty_printer_factory=>get_activ_implementing_classes( ).

    "THEN
    cl_abap_unit_assert=>assert_table_contains( EXPORTING line   = |ZCL_OP_VALUE_PRETTY_PRINTER|
                                                          table  = classes
                                                          msg    = |Main implementation of zif_op_value_pretty_printer was not found|    ).

  ENDMETHOD.


  METHOD create_standard_pretty_printer.

    "GIVEN
    zcl_op_pretty_printer_factory=>user_class_customizing = VALUE #( ( user_name = |AGEPPART|  class_name = |ZCL_OP_VALUE_PRETTY_PRINTER| ) ).

    "WHEN
    DATA(pretty_printer) = zcl_op_pretty_printer_factory=>create( ).


    "THEN
    TRY.
        DATA(must_be_pretty_printer) = CAST zcl_op_value_pretty_printer( pretty_printer ).

      CATCH cx_sy_move_cast_error INTO DATA(error).
        cl_abap_unit_assert=>fail( EXPORTING msg = |Standard class ZCL_OP_VALUE_PRETTY_PRINTER was not used in factory but should have been. Error is { error->get_longtext( ) }| ).
    ENDTRY.


  ENDMETHOD.

ENDCLASS.
