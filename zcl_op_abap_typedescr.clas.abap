CLASS zcl_op_abap_typedescr DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    CONSTANTS typekind_int TYPE abap_typekind VALUE 'I'.    "#EC NOTEXT
    CONSTANTS typekind_int1 TYPE abap_typekind VALUE 'b'.   "#EC NOTEXT
    CONSTANTS typekind_int2 TYPE abap_typekind VALUE 's'.   "#EC NOTEXT
    "in 7.40 there is no int8 but in s4hana there is one
    "having this constants in z-class gives little upward compatibility
    CONSTANTS typekind_int8 TYPE abap_typekind VALUE '8'.   "#EC NOTEXT
    CONSTANTS typekind_struct1 TYPE abap_typekind VALUE 'u'. "#EC NOTEXT
    CONSTANTS typekind_struct2 TYPE abap_typekind VALUE 'v'. "#EC NOTEXT
    CONSTANTS typekind_table TYPE abap_typekind VALUE 'h'.  "#EC NOTEXT
    CONSTANTS typekind_string TYPE abap_typekind VALUE 'g'. "#EC NOTEXT

  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_op_abap_typedescr IMPLEMENTATION.
ENDCLASS.
