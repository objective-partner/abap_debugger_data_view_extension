"! need for dependency injection via lookup and unit tests
"! see openSAP course Writing Testable Code for ABAP Week_5
CLASS zcl_op_pretty_printer_injector DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    CLASS-METHODS:
      inject_pretty_printer             IMPORTING i_pretty_printer         TYPE REF TO zif_op_value_pretty_printer,
      inject_class_names                IMPORTING i_class_names            TYPE string_table,
      inject_user_class_customizing     IMPORTING i_user_class_customizing TYPE zcl_op_pretty_printer_factory=>tt_dve_cust.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_op_pretty_printer_injector IMPLEMENTATION.

  METHOD inject_pretty_printer.
    zcl_op_pretty_printer_factory=>pretty_printer = i_pretty_printer.
  ENDMETHOD.


  METHOD inject_class_names.
    zcl_op_pretty_printer_factory=>class_names = i_class_names.
  ENDMETHOD.


  METHOD inject_user_class_customizing.
    zcl_op_pretty_printer_factory=>user_class_customizing = i_user_class_customizing.
  ENDMETHOD.

ENDCLASS.
