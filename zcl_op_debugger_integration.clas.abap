"!This Class is offering an integration for debugger structure view
CLASS zcl_op_debugger_integration DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    METHODS:
      "! Get reference to structure or table content
      "! we need to do this as the full structure content is hidden here
      "! and need to be retrieved via a kernel call
      "! @parameter i_variable_name | Name of the structure
      "! @parameter r_content_ref   | Reference to structures content
      "! @raising cx_tpda_sys_symb |
      "! @raising cx_tpda_varname |
      "! @raising cx_tpda_internel_error |
      get_ref_to_any_content
        IMPORTING i_variable_name      TYPE string
        RETURNING VALUE(r_content_ref) TYPE REF TO data
        RAISING   cx_tpda_sys_symb
                  cx_tpda_varname
                  cx_tpda_internel_error.

    CLASS-METHODS:
      debug_debugger_if_needed.


  PROTECTED SECTION.
  PRIVATE SECTION.

    METHODS:

      get_data_as_readable_xml
        IMPORTING
                  i_variable_name            TYPE string
        RETURNING
                  VALUE(r_readable_xml_data) TYPE string
        RAISING   cx_tpda_sys_symb,

      create_empty_data_structure
        IMPORTING
                  i_variable_name                   TYPE string
        RETURNING VALUE(r_empty_data_structure_ref) TYPE REF TO data
        RAISING   cx_tpda_sys_symb
                  cx_tpda_varname
                  cx_tpda_internel_error,

      fill_empty_data_from_xml
        IMPORTING i_xml                              TYPE string
                  i_empty_data_structure_ref         TYPE REF TO data
        RETURNING VALUE(r_filled_data_structure_ref) TYPE REF TO data,

      rename_xml_node
        IMPORTING
          i_new_node_name TYPE string
        CHANGING
          c_document      TYPE REF TO if_ixml_document,

      render_new_xml_document
        CHANGING
          c_xml      TYPE string
          c_ixml     TYPE REF TO if_ixml
          c_document TYPE REF TO if_ixml_document.

ENDCLASS.



CLASS ZCL_OP_DEBUGGER_INTEGRATION IMPLEMENTATION.


  METHOD create_empty_data_structure.

    cl_tpda_ctrl_handler=>get_quick_var(  EXPORTING
                                            p_symbname        = i_variable_name     " symbol name
                                          IMPORTING
                                            p_symbquick_ref   = DATA(symbquick_ref) ).

    cl_tpda_variable_services=>convert_quick_info( EXPORTING
                                                      p_symbquick            = symbquick_ref    " TPDA: Retrieval Structure for get_Symb_Quick
                                                      p_ref_flag             = space
                                                      p_itab_body_flag       = space
                                                   IMPORTING
                                                      p_var_result           = DATA(struc_data)    ).




    CREATE DATA r_empty_data_structure_ref TYPE (struc_data-varabstypename).  " RTTC - dynamic creation of  elementary data object

  ENDMETHOD.


  METHOD debug_debugger_if_needed.

    IF sy-datum = '20210302' AND sy-uname = 'R'.
      BREAK-POINT ##NEEDED.
    ENDIF.

  ENDMETHOD.


  METHOD fill_empty_data_from_xml.

    DATA: content_ref TYPE REF TO data.

    content_ref = i_empty_data_structure_ref.


    DATA(xml)            = i_xml. "we need a copy as we will change it locally

    DATA(ixml)           = cl_ixml=>create( ).
    DATA(stream_factory) = ixml->create_stream_factory( ).
    DATA(document)       = ixml->create_document( ).

    CONSTANTS: c_new_node_name TYPE string VALUE 'Here_Is_the_Data'.

    IF ixml->create_parser( document        = document
                            stream_factory  = stream_factory
                            istream         = stream_factory->create_istream_string( xml )
                            )->parse( ) <> 0.
      RETURN.
    ENDIF.

    me->rename_xml_node( EXPORTING  i_new_node_name = c_new_node_name
                         CHANGING   c_document      = document ).

    me->render_new_xml_document(  CHANGING c_xml      = xml
                                           c_ixml     = ixml
                                           c_document = document ).

    DATA: transform_result_binding TYPE abap_trans_resbind_tab.
    FIELD-SYMBOLS: <ls_rtab> LIKE LINE OF transform_result_binding.

    APPEND INITIAL LINE TO transform_result_binding ASSIGNING <ls_rtab>.
    <ls_rtab>-name = c_new_node_name.
    <ls_rtab>-value = content_ref.

    CALL TRANSFORMATION id
      SOURCE XML  xml
           RESULT (transform_result_binding) ##no_text.

    r_filled_data_structure_ref = content_ref.

  ENDMETHOD.


  METHOD get_data_as_readable_xml.

    cl_tpda_ctrl_handler=>get_symb_asxml(  EXPORTING
                                             symbname  = |{ i_variable_name }|
                                             offset    = -1
                                             len       = -1
                                           IMPORTING
                                             xml       = DATA(binary_xml_data) ).
    r_readable_xml_data = cl_abap_codepage=>convert_from( binary_xml_data ).
  ENDMETHOD.


  METHOD get_ref_to_any_content.
    DATA(readable_xml_data) = me->get_data_as_readable_xml( i_variable_name ).

    DATA(empty_data_structure_ref) = me->create_empty_data_structure( i_variable_name  ).

    r_content_ref = me->fill_empty_data_from_xml(  EXPORTING   i_xml                       = readable_xml_data
                                                               i_empty_data_structure_ref  = empty_data_structure_ref    ).
  ENDMETHOD.


  METHOD rename_xml_node.

    DATA last_node_name TYPE string.

    DATA(node_element_filter) = c_document->create_filter_node_type( node_types = if_ixml_node=>co_node_element ).

    DATA(iterator) = c_document->create_iterator_filtered( filter = node_element_filter ).

    DO.
      DATA(node) = iterator->get_next( ).
      IF node IS INITIAL.
        EXIT.
      ENDIF.
      DATA(current_node_name) = node->get_name( ).
      IF last_node_name EQ |values|.
        "we have to rename current node (start element), as its name can lead to unseccessfull transformation
        node->set_name( name = i_new_node_name ).
        EXIT.
      ENDIF.
      last_node_name = current_node_name.
    ENDDO.

  ENDMETHOD.


  METHOD render_new_xml_document.

    CLEAR: c_xml. "we will append new content to it now
    c_document->render( ostream = c_ixml->create_stream_factory( )->create_ostream_cstring( string = c_xml ) ).

  ENDMETHOD.
ENDCLASS.
