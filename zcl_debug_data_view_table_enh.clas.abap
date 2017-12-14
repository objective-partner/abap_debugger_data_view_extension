CLASS zcl_debug_data_view_table_enh DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    "code addition for ALV pushbuttons
    "for placing custom buttons
    METHODS:
      handle_toolbar_set
            FOR EVENT toolbar OF cl_gui_alv_grid
        IMPORTING
            !e_object
            !e_interactive,
      show_popup_w_content
        IMPORTING
          !it_table        TYPE ANY TABLE
          !it_fieldcatalog TYPE lvc_t_fcat
          !iv_table_title  TYPE string .
  PRIVATE SECTION.

    METHODS:
      prepare_output
        IMPORTING
          !it_fieldcatalog      TYPE lvc_t_fcat
          !it_table             TYPE ANY TABLE
          !iv_wrap_from_here    TYPE i OPTIONAL
          !iv_table_title       TYPE string
        RETURNING
          VALUE(rv_string_main) TYPE string,
      get_next_value
        IMPORTING is_field_info       TYPE lvc_s_fcat
                  iv_field_value      TYPE any
        RETURNING VALUE(rv_new_value) TYPE string.

ENDCLASS.



CLASS ZCL_DEBUG_DATA_VIEW_TABLE_ENH IMPLEMENTATION.


  METHOD  get_next_value.
    DATA(lv_value_as_string) = CONV string( iv_field_value ).
    DATA(lv_width) = strlen( lv_value_as_string  ).

    rv_new_value = |{ is_field_info-seltext } = '{ iv_field_value ALPHA = IN  WIDTH = lv_width }|.
  ENDMETHOD.


  METHOD handle_toolbar_set.
    APPEND VALUE stb_button( function = 'ZDATA_4_ABAP_VIEW'  butn_type = 0 text = 'Data for ABAP View' ) TO e_object->mt_toolbar.
  ENDMETHOD.                    "handle_toolbar_set


  METHOD prepare_output.

    FIELD-SYMBOLS: <s_tab_line> TYPE any.
    DATA: lv_string_main              TYPE string,
          lv_string_line              TYPE string,
          lv_wrap_from_here           TYPE i VALUE 255,
          lv_possible_wrap_min_length TYPE i,
          lv_tmp_str                  TYPE string,
          lv_tmp_str_length           TYPE i.


    DATA(lv_wrap_from_text) = |{ 'add line wrap on character number:'(001) }|.

    IF iv_wrap_from_here IS NOT SUPPLIED.
      cl_demo_input=>request( EXPORTING text  = lv_wrap_from_text
                                CHANGING  field = lv_wrap_from_here ).
    ELSE.
      lv_wrap_from_here = iv_wrap_from_here.
    ENDIF.
    lv_possible_wrap_min_length = lv_wrap_from_here.

    rv_string_main = |{ iv_table_title } = VALUE #(\n|.
    LOOP AT it_table ASSIGNING <s_tab_line>.
      rv_string_main = |{ rv_string_main }  ( |.
      "* print columns
      LOOP AT it_fieldcatalog INTO DATA(ls_field_info).
        ASSIGN COMPONENT ls_field_info-fieldname OF STRUCTURE <s_tab_line> TO FIELD-SYMBOL(<v_field>).
        CHECK <v_field> IS ASSIGNED AND
              <v_field> IS NOT INITIAL AND
              ls_field_info-seltext NE |INDEX|.
        DATA(lv_temp_string) = |{ lv_string_line } { get_next_value( is_field_info = ls_field_info  iv_field_value = <v_field> ) }'|. "add column and value like -> MAND = '100'
        DATA(lv_temp_string_length) =  strlen( lv_temp_string ).

        IF lv_temp_string_length > lv_wrap_from_here.
          " new column value combination does not fit and must be placed in a new line
          rv_string_main = |{ rv_string_main }  { lv_string_line }\n|. "* wrap line here
          CLEAR: lv_string_line.
        ENDIF.
        lv_string_line = |{ lv_string_line } { get_next_value( is_field_info = ls_field_info  iv_field_value = <v_field> ) }'\t|.
      ENDLOOP.
      IF lv_string_line NE space.
        rv_string_main = |{ rv_string_main }  { lv_string_line } )\n|. "* wrap line on each new added itab line
      ENDIF.
      CLEAR: lv_string_line.
    ENDLOOP.

    rv_string_main = |{ rv_string_main }\n). |.
  ENDMETHOD.


  METHOD show_popup_w_content.

    DATA lv_string_main TYPE string.

    lv_string_main = prepare_output(
          it_fieldcatalog = it_fieldcatalog
          it_table        = it_table
          iv_table_title = iv_table_title
          ).
    cl_demo_output=>set_mode( cl_demo_output=>text_mode ). "set to text mode to be more compatible with minus signs and so on
    cl_demo_output=>display( lv_string_main ).



  ENDMETHOD.
ENDCLASS.
