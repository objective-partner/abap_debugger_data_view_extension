CLASS zcl_debug_data_view_struc_enh DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    CLASS-METHODS show_popup_w_content
      IMPORTING
        !it_struc_data TYPE tpda_struc_view_it
        !iv_struc_name TYPE string .
  PROTECTED SECTION.
  PRIVATE SECTION.

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
          VALUE(rv_wrap_from_here) TYPE i.
ENDCLASS.



CLASS zcl_debug_data_view_struc_enh IMPLEMENTATION.


  METHOD prepare_output.
    FIELD-SYMBOLS: <s_tab_line> TYPE tpda_struc_view.
    DATA: lv_string_main     TYPE string,
          lv_current_context TYPE string,
          lv_wrap_from_here  TYPE i VALUE 255.

    CHECK it_struc_data[] IS NOT INITIAL.

    lv_wrap_from_here = get_wrap_from_value( iv_wrap_from_here = iv_wrap_from_here  ).

    rv_content_4_display = |{ iv_struc_name } = VALUE #( |.
    LOOP AT it_struc_data ASSIGNING <s_tab_line>.
      "print current column
      CHECK <s_tab_line>-varvalue IS NOT INITIAL AND
            <s_tab_line>-component NE |INDEX|.

      DATA(lv_current_string_length) =  strlen( |{ lv_current_context } { <s_tab_line>-component } = '{ <s_tab_line>-varvalue }'| ).

      IF lv_current_string_length > lv_wrap_from_here.
        " new column value combination does not fit and must be placed in a new line, do that with \r (Carriage Return)
        lv_current_context = |{ lv_current_context } \r { <s_tab_line>-component } = '{ <s_tab_line>-varvalue }'\t|.
      ELSE.
        lv_current_context = |{ lv_current_context } { <s_tab_line>-component } = '{ <s_tab_line>-varvalue }'\t|.
      ENDIF.

      IF lv_current_context NE space.
        rv_content_4_display = |{ rv_content_4_display }  { lv_current_context }|.
      ENDIF.

      CLEAR: lv_current_context.

    ENDLOOP.

    rv_content_4_display = |{ rv_content_4_display } ).|.
  ENDMETHOD.


  METHOD show_popup_w_content.
    DATA: lv_content_4_display TYPE string.
    lv_content_4_display  = prepare_output( iv_struc_name = iv_struc_name
                                            it_struc_data = it_struc_data ).

    cl_demo_output=>set_mode( cl_demo_output=>text_mode ). "set to text mode to be more compatible with minus signs and so on
    cl_demo_output=>display( lv_content_4_display ).
  ENDMETHOD.

  METHOD get_wrap_from_value.

    IF iv_wrap_from_here > 0.
      rv_wrap_from_here = iv_wrap_from_here.
    ELSE.
      DATA(lv_wrap_from_text) = |{ 'add line wrap on character number:'(001) }|.
      cl_demo_input=>request( EXPORTING text  = lv_wrap_from_text
                              CHANGING  field = rv_wrap_from_here ).
    ENDIF.

  ENDMETHOD.

ENDCLASS.
