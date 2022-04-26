"!This Class is offering a simplified field catalog
"!where only needed fields are populated for debugger extension
CLASS zcl_op_simple_field_catalog DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    METHODS:
      "! get field_catalog by reference to data
      "! @parameter i_reference_to_data | reference to current data
      "! @parameter r_field_catalog     | field catalog of current data
      get_by_reference
        IMPORTING i_reference_to_data    TYPE REF TO data
        RETURNING VALUE(r_field_catalog) TYPE lvc_t_fcat,
      "! get field_catalog by given data
      "! @parameter i_reference_to_data | current given data
      "! @parameter r_field_catalog     | field catalog of current data
      get_by_data
        IMPORTING i_table                TYPE ANY TABLE OPTIONAL
                  i_structure            TYPE any OPTIONAL
        RETURNING VALUE(r_field_catalog) TYPE lvc_t_fcat.
  PROTECTED SECTION.
  PRIVATE SECTION.
    METHODS:
      get_fieldcatalog_from_ddic
        IMPORTING
          i_rtti                 TYPE REF TO cl_abap_typedescr
*          i_structure_description TYPE REF TO cl_abap_structdescr
        RETURNING
          VALUE(r_field_catalog) TYPE lvc_t_fcat,
      get_fieldcat_from_local_type
        IMPORTING
          i_rtti                 TYPE REF TO cl_abap_typedescr
*          i_structure_description TYPE REF TO cl_abap_structdescr
        RETURNING
          VALUE(r_field_catalog) TYPE lvc_t_fcat,
      transform
        CHANGING
          !c_ddic_fields TYPE ddfields OPTIONAL
          !c_ddic_field  TYPE dfies OPTIONAL,
      map_to_field_catalog
        IMPORTING
          i_ddic_fields          TYPE ddfields
        RETURNING
          VALUE(r_field_catalog) TYPE lvc_t_fcat,
      get_reference_to_data
        IMPORTING
          i_table       TYPE ANY TABLE
          i_structure   TYPE any
        RETURNING
          VALUE(r_data) TYPE REF TO data,
      get_fieldcatalog_via_reference
        IMPORTING i_reference_to_data    TYPE REF TO data
        RETURNING VALUE(r_field_catalog) TYPE lvc_t_fcat.
ENDCLASS.



CLASS zcl_op_simple_field_catalog IMPLEMENTATION.

  METHOD get_by_reference.
    r_field_catalog = me->get_fieldcatalog_via_reference( i_reference_to_data  ).
  ENDMETHOD.

  METHOD get_fieldcatalog_via_reference.

    DATA(rtti) = cl_abap_typedescr=>describe_by_data_ref( i_reference_to_data ).

    IF rtti->kind = rtti->kind_table.
      rtti = CAST cl_abap_tabledescr( rtti )->get_table_line_type( ).
    ENDIF.

    r_field_catalog = COND #( WHEN rtti->is_ddic_type( ) = abap_true
                              THEN me->get_fieldcatalog_from_ddic( rtti )
                              ELSE me->get_fieldcat_from_local_type( rtti ) ).

  ENDMETHOD.

  METHOD get_by_data.

    DATA(reference_to_data) = me->get_reference_to_data( i_table = i_table  i_structure = i_structure ).

    r_field_catalog = me->get_fieldcatalog_via_reference( reference_to_data  ).
  ENDMETHOD.


  METHOD get_fieldcatalog_from_ddic.

    CASE i_rtti->kind.
      WHEN i_rtti->kind_elem.
        DATA(ddic_fields) = VALUE ddfields( ( CAST cl_abap_elemdescr( i_rtti )->get_ddic_field( ) ) ).
      WHEN i_rtti->kind_struct.
        ddic_fields = CAST cl_abap_structdescr( i_rtti )->get_ddic_field_list( ).
    ENDCASE.

    transform( CHANGING c_ddic_fields = ddic_fields ).

    r_field_catalog = map_to_field_catalog( ddic_fields ).

  ENDMETHOD.


  METHOD get_fieldcat_from_local_type.
    "in case it is a local itab, structure or element definition
    CASE i_rtti->kind.
      WHEN i_rtti->kind_elem.
        APPEND VALUE lvc_s_fcat( LET element_description2 = CAST cl_abap_elemdescr( i_rtti ) IN
                                 inttype    = element_description2->type_kind
                                 decimals   = element_description2->decimals
                                 convexit   = element_description2->edit_mask
                                 outputlen  = element_description2->output_length )
                               TO r_field_catalog.
      WHEN i_rtti->kind_struct.
        LOOP AT CAST cl_abap_structdescr( i_rtti )->get_components( ) INTO DATA(component).
          CASE component-type->kind.
            WHEN 'E'.
              DATA(element_description) = CAST cl_abap_elemdescr( component-type ).
              IF element_description->is_ddic_type( ) = abap_true.
                APPEND VALUE lvc_s_fcat( fieldname  = component-name
                                         seltext    = component-name
                                         inttype    = element_description->type_kind
                                         decimals   = element_description->decimals
                                         convexit   = element_description->edit_mask
                                         outputlen  = element_description->output_length )
                               TO r_field_catalog.

              ELSE.
                APPEND VALUE lvc_s_fcat( fieldname  = component-name
                                         seltext    = component-name
                                         inttype    = element_description->type_kind
                                         decimals   = element_description->decimals
                                         convexit   = element_description->edit_mask
                                         outputlen  = element_description->output_length )
                               TO r_field_catalog.
              ENDIF.
            WHEN 'S'.
              IF component-as_include = abap_true.
                DATA(lt_include_field_cat) = me->get_fieldcat_from_local_type( i_rtti = CAST #( component-type ) ).
                APPEND LINES OF lt_include_field_cat TO r_field_catalog.
              ELSE.
                DATA(structure_description) = CAST cl_abap_structdescr( component-type ).
                APPEND VALUE lvc_s_fcat( fieldname  = component-name
                                         seltext    = component-name
                                         inttype    = structure_description->type_kind
                                         datatype   = |STRU| )
                                 TO r_field_catalog.
              ENDIF.
            WHEN 'T'.
              DATA(table_description) = CAST cl_abap_tabledescr( component-type ).
              APPEND VALUE #(  fieldname = component-name
                               seltext   = component-name
                               inttype   = table_description->type_kind
                               datatype  = COND #( WHEN table_description->kind EQ table_description->kind_table
                                                   THEN |TTYP|
                                                   ELSE space ) )
                               TO r_field_catalog.
          ENDCASE.

        ENDLOOP.
    ENDCASE.
  ENDMETHOD.


  METHOD transform.
    "see CL_SALV_DATA_DESCR=>TRANSFORM
    IF c_ddic_field-tabname EQ c_ddic_field-rollname OR
       c_ddic_field-tabname EQ c_ddic_field-domname.
      CLEAR c_ddic_field-tabname.
    ENDIF.


    c_ddic_field-reftable = c_ddic_field-tabname.
    c_ddic_field-reffield = c_ddic_field-fieldname.
    CLEAR c_ddic_field-precfield.

    FIELD-SYMBOLS: <ddic_field> TYPE dfies.

    LOOP AT c_ddic_fields ASSIGNING <ddic_field>.

      CLEAR <ddic_field>-precfield.
      IF <ddic_field>-datatype = 'CURR' OR <ddic_field>-datatype = 'QUAN'.

        READ TABLE c_ddic_fields WITH KEY fieldname = <ddic_field>-reffield TRANSPORTING NO FIELDS.
        IF sy-subrc EQ 0.
          <ddic_field>-precfield = <ddic_field>-reffield.
        ENDIF.

      ENDIF.
      <ddic_field>-reftable = <ddic_field>-tabname.
      <ddic_field>-reffield = <ddic_field>-fieldname.

    ENDLOOP.
  ENDMETHOD.


  METHOD map_to_field_catalog.
    DATA field_catalog TYPE lvc_s_fcat.

    LOOP AT i_ddic_fields  INTO DATA(ls_dfies).
      CLEAR field_catalog.
      MOVE-CORRESPONDING ls_dfies TO field_catalog.
      field_catalog-seltext = ls_dfies-fieldname.
      APPEND field_catalog TO r_field_catalog.
    ENDLOOP.
  ENDMETHOD.


  METHOD get_reference_to_data.

    IF  i_table[] IS NOT INITIAL.
      CREATE DATA r_data LIKE LINE OF i_table.
    ELSE.
      CREATE DATA r_data LIKE i_structure.
    ENDIF.

  ENDMETHOD.

ENDCLASS.
