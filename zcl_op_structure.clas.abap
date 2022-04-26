"!This class is offering an API for represent a structure
"!in its ABAP VALUE way: structure_name = VALUE #(...)
CLASS zcl_op_structure DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.

    METHODS:
      "! is getting called from enhancement <br/>
      "! will ask for max line length, format content and show it
      "! @parameter i_structure | given structure
      "! @parameter i_field_catalog | field catalog of structure
      "! @parameter i_struc_name | structure name for "name" = VALUE #() statement
      show_popup_w_content
        IMPORTING
                  i_structure     TYPE any
                  i_field_catalog TYPE lvc_t_fcat
                  i_struc_name    TYPE string,
      "! add a structure to given formated context
      "! @parameter i_current_context | current context as string
      "! @parameter i_structure |  given structure
      "! @parameter i_field_catalog | field catalog of structure
      "! @parameter r_current_context | changed context with added structure data as string
      add_structure
        IMPORTING
          i_current_context        TYPE string
          i_structure              TYPE any
          i_field_catalog          TYPE lvc_t_fcat
        RETURNING
          VALUE(r_current_context) TYPE string.
  PROTECTED SECTION.
  PRIVATE SECTION.

    METHODS:
      "! prepare component name
      "! delete surrounding spaces
      component_name  IMPORTING i_component_name        TYPE lvc_fname
                      RETURNING VALUE(r_component_name) TYPE lvc_fname,
      prepare_output
        IMPORTING
                  i_structure                TYPE any
                  i_field_catalog            TYPE lvc_t_fcat
                  i_struc_name               TYPE string
        RETURNING VALUE(r_content_4_display) TYPE string,
      left_hand_side
        IMPORTING
                  i_struc_name                   TYPE string
                  i_is_type_given                TYPE boole_d DEFAULT abap_false
        RETURNING VALUE(rv_left_hand_side_value) TYPE string,
      right_hand_side
        IMPORTING
                  i_is_type_given                TYPE boole_d DEFAULT abap_false
                  i_structure                    TYPE any
                  i_field_catalog                TYPE lvc_t_fcat
        RETURNING VALUE(r_right_hand_side_value) TYPE string,
      handle_nested_structure
        IMPORTING
                  i_current_context        TYPE string
                  i_structure              TYPE any
                  i_component_info         TYPE lvc_s_fcat
        RETURNING VALUE(r_current_context) TYPE string,
      add_rhs_value_prefix
        IMPORTING
                  i_is_type_given                TYPE boole_d
        RETURNING VALUE(r_right_hand_side_value) TYPE string,
      add_rhs_postfix
        IMPORTING
          i_right_hand_side_value        TYPE string
        RETURNING
          VALUE(r_right_hand_side_value) TYPE string,
      add_rhs_value
        IMPORTING i_right_hand_side_value        TYPE string
                  i_structure                    TYPE any
                  i_field_catalog                TYPE lvc_t_fcat
        RETURNING VALUE(r_right_hand_side_value) TYPE string,
      add_rhs_value_and_postfix
        IMPORTING
                  i_right_hand_side_value        TYPE string
                  i_structure                    TYPE any
                  i_field_catalog                TYPE lvc_t_fcat
        RETURNING VALUE(r_right_hand_side_value) TYPE string,
      add_itab
        IMPORTING
                  i_current_context        TYPE string
                  i_table                  TYPE ANY TABLE
                  i_component_name         TYPE lvc_fname
        RETURNING VALUE(r_current_context) TYPE string.

ENDCLASS.



CLASS zcl_op_structure IMPLEMENTATION.


  METHOD component_name.
    r_component_name = i_component_name.
    CONDENSE r_component_name NO-GAPS.
  ENDMETHOD.



  METHOD add_itab.

    r_current_context =   NEW zcl_op_table( )->add_itab(  i_current_context  = ||
                                                          i_table            = i_table
                                                          i_component_name   = i_component_name
                                                          i_dont_add_a_point = abap_true   ).
    r_current_context = |{ i_current_context } { r_current_context }|.
  ENDMETHOD.


  METHOD add_rhs_postfix.

    r_right_hand_side_value = |{ i_right_hand_side_value } )|.

  ENDMETHOD.


  METHOD add_rhs_value.

    DATA current_context TYPE string.

    LOOP AT i_field_catalog INTO DATA(field_info).

      ASSIGN COMPONENT field_info-fieldname OF STRUCTURE i_structure TO FIELD-SYMBOL(<field>).
      CHECK <field> IS ASSIGNED AND
            <field> IS NOT INITIAL.

      current_context = COND #( WHEN field_info-datatype EQ 'TTYP' THEN me->add_itab(   i_current_context = current_context
                                                                                        i_table           = <field>
                                                                                        i_component_name  = field_info-fieldname )
                                WHEN field_info-datatype EQ 'STRU'
                                                                   THEN me->handle_nested_structure(  EXPORTING  i_current_context  = current_context
                                                                                                                 i_structure        = <field>
                                                                                                                 i_component_info   = field_info   )
                                                ELSE  NEW zcl_op_component( )->add( i_current_context = current_context
                                                                                    i_component       = <field>
                                                                                    i_component_info  = field_info  )  ).

    ENDLOOP.

    r_right_hand_side_value = |{ i_right_hand_side_value }{ current_context }|.

  ENDMETHOD.


  METHOD add_rhs_value_and_postfix.

    r_right_hand_side_value =  me->add_rhs_value( EXPORTING
                                                            i_right_hand_side_value = i_right_hand_side_value
                                                            i_field_catalog         = i_field_catalog
                                                            i_structure             = i_structure ).
    r_right_hand_side_value = me->add_rhs_postfix( r_right_hand_side_value ).

  ENDMETHOD.


  METHOD add_rhs_value_prefix.

    r_right_hand_side_value = COND string( WHEN i_is_type_given EQ abap_true
                                                  THEN | = VALUE \{i_type\}(|  "#TODO 25.10.2018 AGEPPART - implement type here
                                              ELSE | = VALUE #(| ).

  ENDMETHOD.


  METHOD add_structure.
    r_current_context = me->right_hand_side(
                                              i_field_catalog         = i_field_catalog
                                              i_structure             = i_structure ).
  ENDMETHOD.



  METHOD handle_nested_structure.
    r_current_context =  right_hand_side(  EXPORTING   i_structure            = i_structure
                                                       i_field_catalog        = NEW zcl_op_simple_field_catalog( )->get_by_data(  i_structure = i_structure ) ).

    r_current_context = |{ i_current_context } { component_name(  i_component_info-fieldname ) }{ r_current_context }|.
  ENDMETHOD.


  METHOD left_hand_side.
    rv_left_hand_side_value = COND string( WHEN i_is_type_given EQ abap_true
                                                THEN |DATA({ i_struc_name })| "#TODO 02.11.2018 AGEPPART - use type later here
                                            ELSE i_struc_name ).
  ENDMETHOD.


  METHOD prepare_output.

    CHECK i_structure IS NOT INITIAL.

    r_content_4_display = |{ me->left_hand_side( i_struc_name  = i_struc_name ) }{ me->right_hand_side( i_structure = i_structure i_field_catalog = i_field_catalog ) }.|.

  ENDMETHOD.


  METHOD right_hand_side.

    r_right_hand_side_value = me->add_rhs_value_prefix( i_is_type_given ).

    r_right_hand_side_value = me->add_rhs_value_and_postfix( EXPORTING i_right_hand_side_value    = r_right_hand_side_value
                                                                       i_structure                = i_structure
                                                                       i_field_catalog            = i_field_catalog   ).

  ENDMETHOD.


  METHOD show_popup_w_content.
    DATA(content_4_display)  = me->prepare_output( i_struc_name = i_struc_name
                                                   i_structure  = i_structure
                                                   i_field_catalog = i_field_catalog ).
    TRY.
        DATA(formated_content) =  zcl_op_pretty_printer_factory=>create( )->format( content_4_display ).
      CATCH  cx_class_not_existent INTO DATA(cx_class_not_existent).
        "formating went wring, fallback using non formated text
        formated_content = content_4_display.
    ENDTRY.
    cl_demo_output=>set_mode( cl_demo_output=>text_mode ). "set to text mode to be more compatible with minus signs and so on
    cl_demo_output=>write_data( formated_content ).
    cl_demo_output=>display( ).
  ENDMETHOD.


ENDCLASS.
