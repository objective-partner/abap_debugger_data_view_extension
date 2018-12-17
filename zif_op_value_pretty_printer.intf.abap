"! Interface for pretty printing a value #() construct
INTERFACE zif_op_value_pretty_printer
  PUBLIC .

  METHODS:
    "! Method for formating given string value,<br/>
    "! which should already be a syntax correct representation of an value #() construct<br/>
    "! See unit test of zcl_op_value_pretty_printer class for usage scenarios<br/>
    "! @parameter i_unformated_value_content | syntax correct representation of an value #() construct
    "! @parameter r_formated_content | formated value #() construct containing line breaks, spaces and so on
    format
      IMPORTING i_unformated_value_content TYPE string
      RETURNING VALUE(r_formated_content)  TYPE string.

ENDINTERFACE.
