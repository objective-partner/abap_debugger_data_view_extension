"!This class is offering an API for representing a table
"!in its ABAP VALUE way: table_name = VALUE #( (...) )
CLASS zcl_op_table DEFINITION
  PUBLIC FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    METHODS:
      handle_toolbar_set FOR EVENT toolbar OF cl_gui_alv_grid IMPORTING e_object
                                   e_interactive,
      show_popup_w_content IMPORTING i_table        TYPE ANY TABLE
                                     i_fieldcatalog TYPE lvc_t_fcat OPTIONAL
                                     i_table_title  TYPE string,
      add_itab IMPORTING i_table_title            TYPE string     OPTIONAL
                         i_current_context        TYPE string
                         i_table                  TYPE ANY TABLE
                         i_component_name         TYPE lvc_fname  OPTIONAL
                         i_fieldcatalog           TYPE lvc_t_fcat OPTIONAL
                         i_dont_add_a_point       TYPE boole_d    OPTIONAL
               RETURNING VALUE(r_current_context) TYPE string,
      filter_table_from_alv IMPORTING i_alv         TYPE REF TO cl_gui_alv_grid OPTIONAL
                                      i_table       TYPE ANY TABLE
                            RETURNING VALUE(result) TYPE REF TO data.

  PRIVATE SECTION.
    METHODS:
      prepare_output IMPORTING i_fieldcatalog             TYPE lvc_t_fcat OPTIONAL
                               i_table                    TYPE ANY TABLE
                               i_table_title              TYPE string
                               i_dont_add_a_point         TYPE boole_d    OPTIONAL
                     RETURNING VALUE(r_content_4_display) TYPE string,
      add_prefix IMPORTING i_tabix                  TYPE i
                           i_current_context        TYPE string
                           i_table_title            TYPE string    OPTIONAL
                           i_component_name         TYPE lvc_fname OPTIONAL
                 RETURNING VALUE(r_current_context) TYPE string.
ENDCLASS.


CLASS zcl_op_table IMPLEMENTATION.
  METHOD add_itab.
    DATA table_line_context TYPE string.

    TRY.

        DATA(simple_field_catalog) = NEW zcl_op_simple_field_catalog( ).
        DATA(field_catalog) = COND lvc_t_fcat( WHEN i_fieldcatalog[] IS NOT INITIAL
                                               THEN i_fieldcatalog
                                               ELSE simple_field_catalog->get_by_data( i_table = i_table  ) ).

        DATA(structure) = NEW zcl_op_structure( ).
        DATA(component) = NEW zcl_op_component( ).

        r_current_context = COND string( WHEN i_current_context IS NOT INITIAL THEN i_current_context ELSE || ).

        DATA(line_num) = 0.
        LOOP AT i_table ASSIGNING FIELD-SYMBOL(<table_line>).

          line_num += 1.
          r_current_context = add_prefix( i_current_context = r_current_context
                                          i_table_title     = i_table_title
                                          i_component_name  = i_component_name
                                          i_tabix           = line_num ).
          " print columns
          LOOP AT field_catalog INTO DATA(field_info)
               WHERE no_out = abap_false.
            IF lines( field_catalog ) = 1 AND field_catalog[ 1 ]-fieldname IS INITIAL.
              ASSIGN <table_line> TO FIELD-SYMBOL(<field>).
            ELSE.
              ASSIGN COMPONENT field_info-fieldname OF STRUCTURE <table_line> TO <field>.
            ENDIF.
            CHECK     <field> IS ASSIGNED
                  AND <field> IS NOT INITIAL.

            DATA(varvalue) = COND tpda_var_value( WHEN field_info-datatype = |TTYP| THEN me->add_itab(
                                                      i_current_context  = space
                                                      i_table            = <field>
                                                      i_dont_add_a_point = abap_true )
                                                  WHEN field_info-datatype = |STRU| THEN structure->add_structure(
                                                      i_structure       = <field>
                                                      i_field_catalog   = simple_field_catalog->get_by_data(
                                                                              i_structure = <field> )
                                                      i_current_context = || )
                                                  ELSE <field>   ). " it must be a component

            table_line_context =  component->add( i_datatype        = field_info-datatype
                                                  i_current_context = table_line_context
                                                  i_component       = varvalue         " here we only pass already given value as string
                                                  i_component_info  = field_info  ).

          ENDLOOP.
          IF table_line_context <> space.
            r_current_context = |{ r_current_context } ({ table_line_context } )|.
          ENDIF.
          CLEAR table_line_context.
        ENDLOOP.

        r_current_context = COND #( WHEN i_dont_add_a_point = abap_true
                                    THEN |{ r_current_context } )|
                                    ELSE |{ r_current_context } ).| ).

      CATCH cx_root INTO DATA(lx_root). " TODO: variable is assigned but never used (ABAP cleaner)
        ASSERT 1 = 1.
    ENDTRY.
  ENDMETHOD.

  METHOD add_prefix.
    r_current_context = COND string( WHEN i_current_context IS NOT INITIAL THEN i_current_context ELSE || ).

    IF i_tabix = 1.
      r_current_context = COND #( WHEN i_table_title <> space          THEN |{ r_current_context }{ i_table_title } = VALUE #(|
                                  WHEN i_component_name IS NOT INITIAL THEN |{ r_current_context  }{ i_component_name } = VALUE #(|
                                  ELSE |{ r_current_context }VALUE #(| ).
    ENDIF.
  ENDMETHOD.

  METHOD handle_toolbar_set.
    CHECK NOT line_exists( e_object->mt_toolbar[ function = 'ZDATA_4_ABAP_VIEW' ] ).

    APPEND VALUE stb_button( function  = 'ZDATA_4_ABAP_VIEW'
                             butn_type = 0
                             text      = 'Data for ABAP View' ) TO e_object->mt_toolbar.
  ENDMETHOD.

  METHOD prepare_output.
    r_content_4_display = add_itab( i_table_title      = i_table_title
                                    i_current_context  = space
                                    i_table            = i_table
                                    i_fieldcatalog     = i_fieldcatalog
                                    i_dont_add_a_point = i_dont_add_a_point    ).
  ENDMETHOD.

  METHOD show_popup_w_content.
    DATA(plain_content) = prepare_output( i_fieldcatalog = i_fieldcatalog
                                          i_table        = i_table
                                          i_table_title  = i_table_title ).

    TRY.
        DATA(formated_content) = zcl_op_pretty_printer_factory=>create( )->format( plain_content ).
      CATCH cx_class_not_existent INTO DATA(cx_class_not_existent). " TODO: variable is assigned but never used (ABAP cleaner)
        " formatting went wrong, fallback using non formatted text
        formated_content = plain_content.
    ENDTRY.

    DATA(content_4_display) = COND #( WHEN formated_content IS NOT INITIAL THEN formated_content
                                      WHEN plain_content IS NOT INITIAL    THEN plain_content ).

    cl_demo_output=>set_mode( cl_demo_output=>text_mode ). " set to text mode to be more compatible with minus signs and so on
    cl_demo_output=>write_data( content_4_display ).
    cl_demo_output=>display( ).
  ENDMETHOD.

  METHOD filter_table_from_alv.
    DATA filtered_entries_std TYPE TABLE OF i.

    FIELD-SYMBOLS <filtered_table> TYPE table.

    " create local copy of table for filtering
    CREATE DATA result LIKE i_table.
    ASSIGN result->* TO <filtered_table>.
    <filtered_table> = i_table.

    IF i_alv IS NOT BOUND.
      RETURN.
    ENDIF.

    i_alv->get_filtered_entries( IMPORTING et_filtered_entries = DATA(filtered_entries) ).
    filtered_entries_std = filtered_entries.
    " sort descending so deletion of entries doesn't mess up the table indices
    SORT filtered_entries_std BY table_line DESCENDING.
    LOOP AT filtered_entries_std ASSIGNING FIELD-SYMBOL(<i>).
      DELETE <filtered_table> INDEX <i>.
    ENDLOOP.
  ENDMETHOD.
ENDCLASS.
