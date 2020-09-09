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
          i_structure_description TYPE REF TO cl_abap_structdescr
        RETURNING
          VALUE(r_field_catalog)  TYPE lvc_t_fcat,
      get_fieldcat_from_local_type
        IMPORTING
          i_structure_description TYPE REF TO cl_abap_structdescr
        RETURNING
          VALUE(r_field_catalog)  TYPE lvc_t_fcat,
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

    TRY.
        DATA(structure_description) = CAST cl_abap_structdescr( cl_abap_structdescr=>describe_by_data_ref( i_reference_to_data ) ).
      CATCH  cx_sy_move_cast_error.
        DATA(table_description) = CAST cl_abap_tabledescr( cl_abap_tabledescr=>describe_by_data_ref( i_reference_to_data ) ).
        structure_description = CAST cl_abap_structdescr( table_description->get_table_line_type( ) ).
    ENDTRY.

    r_field_catalog = COND #( WHEN structure_description->is_ddic_type( ) = abap_true
                                                    THEN me->get_fieldcatalog_from_ddic( structure_description )
                                          ELSE me->get_fieldcat_from_local_type( structure_description ) ).

  ENDMETHOD.

  METHOD get_by_data.

    DATA(reference_to_data) = me->get_reference_to_data( i_table = i_table  i_structure = i_structure ).

    r_field_catalog = me->get_fieldcatalog_via_reference( reference_to_data  ).
  ENDMETHOD.


  METHOD get_fieldcatalog_from_ddic.

    DATA(ddic_fields) = i_structure_description->get_ddic_field_list( ).

    transform( CHANGING c_ddic_fields = ddic_fields ).

    r_field_catalog = map_to_field_catalog( ddic_fields ).

  ENDMETHOD.


  METHOD get_fieldcat_from_local_type.
    "in case it is a local itab, structure or element definition
    LOOP AT i_structure_description->get_components( ) INTO DATA(component).
      CASE component-type->kind.
        WHEN 'E'.
          DATA(element_description) = CAST cl_abap_elemdescr( component-type ).
          IF element_description->is_ddic_type( ) = abap_true.
            APPEND VALUE lvc_s_fcat( fieldname  = component-name
                                     seltext    = component-name
                                     inttype    = element_description->type_kind
                                     decimals   = element_description->decimals
                                     convexit   = element_description->edit_mask
                                     outputlen  = element_description->output_length ) TO r_field_catalog.

          ELSE.
            APPEND VALUE lvc_s_fcat( fieldname  = component-name
                                     seltext    = component-name
                                     inttype    = element_description->type_kind
                                     decimals   = element_description->decimals
                                     convexit   = element_description->edit_mask
                                     outputlen  = element_description->output_length ) TO r_field_catalog.
          ENDIF.
        WHEN 'S'.
          if component-as_include = abap_true.
            data(lt_aux) = me->get_fieldcat_from_local_type( i_structure_description = cast #( component-type ) ).
            append lines of lt_aux to r_field_catalog.
          else.
            DATA(structure_description) = CAST cl_abap_structdescr( component-type ).
            APPEND VALUE lvc_s_fcat( fieldname  = component-name
                                     seltext    = component-name
                                     inttype    = structure_description->type_kind
                                     datatype   = |STRU| ) TO r_field_catalog.
          endif.
        WHEN 'T'.
          DATA(table_description) = CAST cl_abap_tabledescr( component-type ).
          APPEND VALUE #(  fieldname = component-name
                           seltext   = component-name
                           inttype   = table_description->type_kind
                           datatype  = COND #( WHEN table_description->kind EQ |T| THEN |TTYP| ELSE space ) ) TO r_field_catalog.
      ENDCASE.

    ENDLOOP.
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
