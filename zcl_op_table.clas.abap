"!This class is offering an API for represent a table
"!in its ABAP VALUE way: table_name = VALUE #( (...) )
CLASS zcl_op_table DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    METHODS:
      handle_toolbar_set
            FOR EVENT toolbar OF cl_gui_alv_grid
        IMPORTING
            e_object
            e_interactive,
      show_popup_w_content
        IMPORTING
          i_table        TYPE ANY TABLE
          i_fieldcatalog TYPE lvc_t_fcat OPTIONAL
          i_table_title  TYPE string,
      add_itab
        IMPORTING
                  i_table_title            TYPE string     OPTIONAL
                  i_current_context        TYPE string
                  i_table                  TYPE ANY TABLE
                  i_component_name         TYPE lvc_fname  OPTIONAL
                  i_fieldcatalog           TYPE lvc_t_fcat OPTIONAL
                  i_dont_add_a_point       TYPE boole_d    OPTIONAL
        RETURNING VALUE(r_current_context) TYPE string.

  PRIVATE SECTION.
    METHODS:
      prepare_output
        IMPORTING
          i_fieldcatalog             TYPE lvc_t_fcat OPTIONAL
          i_table                    TYPE ANY TABLE
          i_table_title              TYPE string
          i_dont_add_a_point         TYPE boole_d OPTIONAL
        RETURNING
          VALUE(r_content_4_display) TYPE string,
      add_prefix
        IMPORTING
                  i_tabix                  TYPE i
                  i_current_context        TYPE string
                  i_table_title            TYPE string       OPTIONAL
                  i_component_name         TYPE lvc_fname    OPTIONAL
        RETURNING VALUE(r_current_context) TYPE string.
ENDCLASS.



CLASS zcl_op_table IMPLEMENTATION.


  METHOD add_itab.
    DATA: table_line_context TYPE string.

    TRY.

        DATA(simple_field_catalog) = NEW zcl_op_simple_field_catalog( ).
        DATA(field_catalog) = COND lvc_t_fcat( WHEN i_fieldcatalog[] IS NOT INITIAL  THEN i_fieldcatalog ELSE simple_field_catalog->get_by_data( i_table = i_table  ) ).

        DATA(structure) = NEW zcl_op_structure( ).
        DATA(component) = NEW zcl_op_component( ).

        r_current_context = COND string( WHEN i_current_context IS NOT INITIAL THEN i_current_context ELSE || ).

        LOOP AT i_table ASSIGNING FIELD-SYMBOL(<table_line>).

          r_current_context = me->add_prefix(  i_current_context = r_current_context
                                               i_table_title     = i_table_title
                                               i_component_name  = i_component_name
                                               i_tabix           = sy-tabix ).
          "print columns
          LOOP AT field_catalog INTO DATA(field_info).
            IF lines( field_catalog ) = 1 AND field_catalog[ 1 ]-fieldname IS INITIAL.
              ASSIGN <table_line> TO FIELD-SYMBOL(<field>).
            ELSE.
              ASSIGN COMPONENT field_info-fieldname OF STRUCTURE <table_line> TO <field>.
            ENDIF.
            CHECK <field> IS ASSIGNED AND
                  <field> IS NOT INITIAL AND
                  field_info-seltext NE |INDEX|.

            DATA(varvalue) = COND tpda_var_value( WHEN field_info-datatype = |TTYP|
                                                        THEN me->add_itab( i_current_context  = space
                                                                           i_table            = <field>
                                                                           i_dont_add_a_point = abap_true )
                                                  WHEN field_info-datatype = |STRU|
                                                        THEN structure->add_structure( i_structure       = <field>
                                                                                       i_field_catalog   = simple_field_catalog->get_by_data( i_structure = <field> )
                                                                                       i_current_context = || )
                                                  ELSE <field>   ). "it must be a component

            table_line_context =  component->add( i_datatype        = field_info-datatype
                                                  i_current_context = table_line_context
                                                  i_component       = varvalue         "here we only pass already given value as string
                                                  i_component_info  = field_info  ).

          ENDLOOP.
          IF table_line_context NE space.
            r_current_context = |{ r_current_context } ({ table_line_context } )|.
          ENDIF.
          CLEAR: table_line_context.
        ENDLOOP.


        r_current_context = COND #( WHEN i_dont_add_a_point EQ abap_true THEN |{ r_current_context } )|
                                                                         ELSE |{ r_current_context } ).| ).

      CATCH cx_root INTO DATA(lx_root).
        ASSERT 1 = 1.
    ENDTRY.

  ENDMETHOD.


  METHOD add_prefix.

    r_current_context = COND string( WHEN i_current_context IS NOT INITIAL THEN i_current_context ELSE || ).

    IF i_tabix  EQ 1.
      r_current_context = COND #( WHEN i_table_title <> space THEN |{ r_current_context }{ i_table_title } = VALUE #(|
                                  WHEN i_component_name IS NOT INITIAL THEN |{ r_current_context  }{ i_component_name } = VALUE #(|
                                  ELSE |{ r_current_context }VALUE #(| ).
    ENDIF.

  ENDMETHOD.


  METHOD handle_toolbar_set.

    CHECK NOT line_exists( e_object->mt_toolbar[ function = 'ZDATA_4_ABAP_VIEW' ] ).

    APPEND VALUE stb_button( function = 'ZDATA_4_ABAP_VIEW'  butn_type = 0 text = 'Data for ABAP View' ) TO e_object->mt_toolbar.

  ENDMETHOD.                   "handle_toolbar_set


  METHOD prepare_output.
    r_content_4_display = me->add_itab( EXPORTING
                                               i_table_title        = i_table_title
                                               i_current_context    = space
                                               i_table              = i_table
                                               i_fieldcatalog       = i_fieldcatalog
                                               i_dont_add_a_point   = i_dont_add_a_point    ).
  ENDMETHOD.


  METHOD show_popup_w_content.
    DATA(content_4_display) = me->prepare_output( i_fieldcatalog = i_fieldcatalog
                                                  i_table        = i_table
                                                  i_table_title  = i_table_title ).

*  DATA lt_string TYPE TABLE OF string.
*  DATA l_filename TYPE string.
*
*  l_filename = 'C:\Users\sandra.rossi\Downloads\abap_init_itab.txt'.
*
*  APPEND content_4_display TO lt_string.
*
*  CALL FUNCTION 'GUI_DOWNLOAD'
*    EXPORTING
*      filename = l_filename
*      filetype = 'ASC'
*    TABLES
*      data_tab = lt_string
*    EXCEPTIONS
*      OTHERS   = 17.
*
*FREE lt_string.

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
