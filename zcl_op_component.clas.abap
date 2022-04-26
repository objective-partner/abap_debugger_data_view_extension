"! Component representation for Abap Debugger Enhancement
"! is responsible for representing components/fields
"! within a -component = 'component value' statement
CLASS zcl_op_component DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    METHODS:

      "! add a single component or already formated context
      "! @parameter i_datatype | datatype of component
      "! @parameter i_current_context | current context as string
      "! @parameter i_component | current component
      "! @parameter i_component_info | field catalog info for this component
      "! @parameter r_current_context | changed context with added component data as string
      add
        IMPORTING
                  i_datatype               TYPE datatype_d  OPTIONAL
                  i_current_context        TYPE string
                  i_component              TYPE any
                  i_component_info         TYPE lvc_s_fcat
        RETURNING VALUE(r_current_context) TYPE string.

  PROTECTED SECTION.
  PRIVATE SECTION.
    METHODS:
      "! prepare component name
      "! delete surrounding spaces
      component_name  IMPORTING i_component_name        TYPE string
                      RETURNING VALUE(r_component_name) TYPE string.
ENDCLASS.



CLASS zcl_op_component IMPLEMENTATION.


  METHOD add.

    r_current_context = i_current_context.

    CHECK i_component IS NOT INITIAL.

    DATA(assign_component_value) = SWITCH #( i_component_info-inttype
                WHEN zcl_op_abap_typedescr=>typekind_int1
                  OR zcl_op_abap_typedescr=>typekind_int2
                  OR zcl_op_abap_typedescr=>typekind_int
                  OR zcl_op_abap_typedescr=>typekind_int8
                THEN condense( |{ i_component }| )
                WHEN zcl_op_abap_typedescr=>typekind_struct1
                  OR zcl_op_abap_typedescr=>typekind_struct2
                  OR zcl_op_abap_typedescr=>typekind_table
                THEN i_component
                WHEN zcl_op_abap_typedescr=>typekind_string
                THEN |`{ replace( val = i_component sub = |`| with = |``| occ = 0 ) }`|
                ELSE |'{ replace( val = i_component sub = |'| with = |''| occ = 0 ) }'| ).

    IF i_component_info-fieldname IS NOT INITIAL.

      DATA(component_name) = COND string( WHEN i_component_info-seltext <> space
                                              AND i_component_info-seltext <> i_component_info-fieldname
                                          THEN i_component_info-seltext
                                          ELSE i_component_info-fieldname ).

      DATA(new_column_value_combi) = | { me->component_name( component_name )
                                       }{ COND #( WHEN i_component_info-datatype <> |STRU| THEN | = | )
                                       }{ assign_component_value }|.

      r_current_context = |{ i_current_context }{ new_column_value_combi }|.

    ELSE.

      " Line type of table has NO component (like a table of integers: DATA itab TYPE TABLE OF i)
      r_current_context = | { assign_component_value }|.

    ENDIF.



  ENDMETHOD.


  METHOD component_name.
    r_component_name = i_component_name.
    CONDENSE r_component_name NO-GAPS.
  ENDMETHOD.
ENDCLASS.
