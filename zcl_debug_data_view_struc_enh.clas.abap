CLASS zcl_debug_data_view_struc_enh DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    CLASS-METHODS show_popup_w_content
      IMPORTING
        !it_struc_data TYPE tpda_struc_view_it
        !iv_struc_name TYPE string .
    CLASS-METHODS current_context
      IMPORTING
        iv_current_context        TYPE string
        iv_wrap_from_here         TYPE i
        i_tab_line                TYPE tpda_struc_view
      RETURNING
        VALUE(rv_current_context) TYPE string.
  PROTECTED SECTION.
  PRIVATE SECTION.

    CONSTANTS:
      c_newline TYPE abap_char1 VALUE cl_abap_char_utilities=>cr_lf,
      c_tab     TYPE c          VALUE cl_abap_char_utilities=>horizontal_tab.

    CLASS-METHODS:
      prepare_output
        IMPORTING
          !it_struc_data              TYPE tpda_struc_view_it
          !iv_wrap_from_here          TYPE i OPTIONAL
          !iv_struc_name              TYPE string
        RETURNING
          VALUE(rv_content_4_display) TYPE string,
      get_wrap_from_value
        IMPORTING
          iv_wrap_from_here        TYPE i OPTIONAL
        RETURNING
          VALUE(rv_wrap_from_here) TYPE i,
      current_string_length
        IMPORTING
          iv_current_context              TYPE string
          iv_tab_line                     TYPE tpda_struc_view
        RETURNING
          VALUE(rv_current_string_length) TYPE i.
ENDCLASS.



CLASS ZCL_DEBUG_DATA_VIEW_STRUC_ENH IMPLEMENTATION.


  METHOD current_context.

    "calculate temp context length from last TAB offset position
    DATA(lv_current_string_length) = current_string_length( iv_current_context = iv_current_context iv_tab_line = i_tab_line ).

    IF lv_current_string_length > iv_wrap_from_here.
      " new column value combination does not fit and must be placed in a new line, do that with { c_newline } (Carriage Return)
      rv_current_context = |{ iv_current_context }{ c_newline }{ i_tab_line-component } = '{ i_tab_line-varvalue }'{ c_tab }|.
    ELSE.
      rv_current_context = |{ iv_current_context }{ i_tab_line-component } = '{ i_tab_line-varvalue }'{ c_tab }|.
    ENDIF.

  ENDMETHOD.


  METHOD current_string_length.

    FIND ALL OCCURRENCES OF c_newline IN iv_current_context MATCH OFFSET DATA(l_newline_offset).
    rv_current_string_length  = strlen( iv_current_context ).
    DATA(lv_tmp_context) = substring( val = iv_current_context off = l_newline_offset len = ( rv_current_string_length - l_newline_offset ) ).


    lv_tmp_context = |{ lv_tmp_context }{ iv_tab_line-component } = '{ iv_tab_line-varvalue }'{ c_tab }|.
    "do the trick with \t and count it as 4 char spaces instead as one char
    "todo so replace it temporarly
    REPLACE ALL OCCURRENCES OF  c_tab IN  lv_tmp_context WITH |    |.
    rv_current_string_length =  strlen( lv_tmp_context ).

  ENDMETHOD.


  METHOD get_wrap_from_value.

    IF iv_wrap_from_here > 0.
      rv_wrap_from_here = iv_wrap_from_here.
    ELSE.
      rv_wrap_from_here = 170.
      DATA(lv_wrap_from_text) = |{ 'add line wrap on character number:'(001) }|.
      cl_demo_input=>request( EXPORTING text  = lv_wrap_from_text
                              CHANGING  field = rv_wrap_from_here ).
    ENDIF.

  ENDMETHOD.


  METHOD prepare_output.
    FIELD-SYMBOLS: <s_tab_line> TYPE tpda_struc_view.
    DATA: lv_string_main     TYPE string,
          lv_current_context TYPE string,
          lv_wrap_from_here  TYPE i VALUE 255.

    CHECK it_struc_data[] IS NOT INITIAL.

    lv_wrap_from_here = get_wrap_from_value( iv_wrap_from_here = iv_wrap_from_here  ).

    rv_content_4_display = |{ iv_struc_name } = VALUE #({ c_tab }|.
    LOOP AT it_struc_data ASSIGNING <s_tab_line>.
      "print current column
      CHECK <s_tab_line>-varvalue IS NOT INITIAL AND
            <s_tab_line>-component NE |INDEX|.

      lv_current_context = current_context(
                                            iv_current_context    = lv_current_context
                                            iv_wrap_from_here     = lv_wrap_from_here
                                            i_tab_line            = <s_tab_line> ).

    ENDLOOP.

    rv_content_4_display = |{ rv_content_4_display }{ lv_current_context }).|.
  ENDMETHOD.


  METHOD show_popup_w_content.
    DATA: lv_content_4_display TYPE string.
    lv_content_4_display  = prepare_output( iv_struc_name = iv_struc_name
                                            it_struc_data = it_struc_data ).

    cl_demo_output=>set_mode( cl_demo_output=>text_mode ). "set to text mode to be more compatible with minus signs and so on
    cl_demo_output=>display( lv_content_4_display ).
  ENDMETHOD.
ENDCLASS.
