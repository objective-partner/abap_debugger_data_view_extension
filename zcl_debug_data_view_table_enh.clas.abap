CLASS zcl_debug_data_view_table_enh DEFINITION
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
          it_table        TYPE ANY TABLE
          it_fieldcatalog TYPE lvc_t_fcat
          iv_table_title  TYPE string.

  PRIVATE SECTION.
    CONSTANTS:
      c_newline TYPE abap_char1 VALUE cl_abap_char_utilities=>cr_lf,
      c_tab     TYPE c          VALUE cl_abap_char_utilities=>horizontal_tab.

    METHODS:
      prepare_output
        IMPORTING
          it_fieldcatalog             TYPE lvc_t_fcat
          it_table                    TYPE ANY TABLE
          iv_wrap_from_here           TYPE i OPTIONAL
          iv_table_title              TYPE string
        RETURNING
          VALUE(rv_content_4_display) TYPE string.
ENDCLASS.



CLASS zcl_debug_data_view_table_enh IMPLEMENTATION.


  METHOD handle_toolbar_set.

    CHECK NOT line_exists( e_object->mt_toolbar[ function = 'ZDATA_4_ABAP_VIEW' ] ).

    APPEND VALUE stb_button( function = 'ZDATA_4_ABAP_VIEW'  butn_type = 0 text = 'Data for ABAP View' ) TO e_object->mt_toolbar.

  ENDMETHOD.                   "handle_toolbar_set


  METHOD prepare_output.

    FIELD-SYMBOLS: <s_tab_line> TYPE any.
    DATA: lv_string_main              TYPE string,
          lv_current_context          TYPE string,
          lv_wrap_from_here           TYPE i VALUE 255,
          lv_possible_wrap_min_length TYPE i,
          lv_tmp_str                  TYPE string,
          lv_tmp_str_length           TYPE i.


    DATA(lo_view_struc) = NEW zcl_debug_data_view_struc_enh( ).
    DATA(lv_wrap_from_text) = |{ 'add line wrap on character number:'(001) }|.

    IF iv_wrap_from_here IS NOT SUPPLIED.
      cl_demo_input=>request( EXPORTING text  = lv_wrap_from_text
                                CHANGING  field = lv_wrap_from_here ).
    ELSE.
      lv_wrap_from_here = iv_wrap_from_here.
    ENDIF.
    lv_possible_wrap_min_length = lv_wrap_from_here.

    rv_content_4_display = |{ iv_table_title } = VALUE #({ c_newline }|.
    LOOP AT it_table ASSIGNING <s_tab_line>.
      rv_content_4_display = |{ rv_content_4_display }({ c_tab }|.
      "* print columns
      LOOP AT it_fieldcatalog INTO DATA(ls_field_info).
        ASSIGN COMPONENT ls_field_info-fieldname OF STRUCTURE <s_tab_line> TO FIELD-SYMBOL(<v_field>).
        CHECK <v_field> IS ASSIGNED AND
              <v_field> IS NOT INITIAL AND
              ls_field_info-seltext NE |INDEX|.

        lv_current_context = lo_view_struc->add_component(
                               iv_current_context = lv_current_context
                               iv_wrap_from_here  = lv_wrap_from_here
                               i_component        = VALUE #( component = ls_field_info-seltext varvalue = <v_field> )  ).
      ENDLOOP.
      IF lv_current_context NE space.
        rv_content_4_display = |{ rv_content_4_display }{ lv_current_context }){ c_newline }|. "* wrap line on each new added itab line
      ENDIF.
      CLEAR: lv_current_context.
    ENDLOOP.

    rv_content_4_display = |{ rv_content_4_display }{ c_tab }).|.
  ENDMETHOD.


  METHOD show_popup_w_content.

    DATA lv_content_4_display TYPE string.

    lv_content_4_display = prepare_output( it_fieldcatalog = it_fieldcatalog
                                           it_table        = it_table
                                           iv_table_title = iv_table_title
                                         ).
    cl_demo_output=>set_mode( cl_demo_output=>text_mode ). "set to text mode to be more compatible with minus signs and so on
    cl_demo_output=>display( lv_content_4_display ).

  ENDMETHOD.
ENDCLASS.
