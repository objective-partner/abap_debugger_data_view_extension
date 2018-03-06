CLASS zcl_debug_data_view_struc_enh DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.

    METHODS:
      show_popup_w_content
        IMPORTING
          !it_struc_data TYPE tpda_struc_view_it
          !iv_struc_name TYPE string,
      add_component
        IMPORTING
          iv_current_context        TYPE string
          iv_wrap_from_here         TYPE i
          i_component               TYPE tpda_struc_view
        RETURNING
          VALUE(rv_current_context) TYPE string.

  PROTECTED SECTION.
  PRIVATE SECTION.

    CONSTANTS:
      c_newline TYPE abap_char1 VALUE cl_abap_char_utilities=>cr_lf,
      c_tab     TYPE c          VALUE cl_abap_char_utilities=>horizontal_tab.

    METHODS:
      component_name
        IMPORTING iv_component_name        TYPE tpda_struc_comp
        RETURNING VALUE(rv_component_name) TYPE tpda_struc_comp,
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
          i_component                     TYPE tpda_struc_view
        RETURNING
          VALUE(rv_current_string_length) TYPE i,
      left_hand_side
        IMPORTING
                  iv_struc_name                  TYPE string
                  iv_is_type_given               TYPE boole_d DEFAULT abap_false
        RETURNING VALUE(rv_left_hand_side_value) TYPE string,
      right_hand_side
        IMPORTING
                  iv_is_type_given                TYPE boole_d DEFAULT abap_false
                  it_struc_data                   TYPE tpda_struc_view_it
                  iv_wrap_from_here               TYPE i
        RETURNING VALUE(rv_right_hand_side_value) TYPE string,
      handle_nested_structure
        IMPORTING
          iv_current_context        TYPE string
          iv_wrap_from_here         TYPE i
          is_component              TYPE tpda_struc_view
        CHANGING
          ct_struc_data             TYPE tpda_struc_view_it
        RETURNING
          VALUE(rv_current_context) TYPE string,
      extract_nested_structure
        IMPORTING
          is_component           TYPE tpda_struc_view
        CHANGING
          ct_struc_data          TYPE tpda_struc_view_it
        RETURNING
          VALUE(rt_nested_struc) TYPE tpda_struc_view_it.

ENDCLASS.



CLASS zcl_debug_data_view_struc_enh IMPLEMENTATION.


  METHOD add_component.

    rv_current_context = iv_current_context.

    CHECK i_component-varvalue IS NOT INITIAL AND
          i_component-component NE |INDEX|.

    "calculate temp context length from last TAB offset position
    DATA(lv_current_string_length) = current_string_length( iv_current_context = iv_current_context i_component = i_component ).

    IF lv_current_string_length > iv_wrap_from_here.
      " new column value combination does not fit and must be placed in a new line, do that with { c_newline } (Carriage Return)
      rv_current_context = |{ iv_current_context }{ c_newline }{ component_name( i_component-component ) } = '{ i_component-varvalue }'{ c_tab }|.
    ELSE.
      rv_current_context = |{ iv_current_context }{ component_name( i_component-component ) } = '{ i_component-varvalue }'{ c_tab }|.
    ENDIF.

  ENDMETHOD.


  METHOD current_string_length.

    FIND ALL OCCURRENCES OF c_newline IN iv_current_context MATCH OFFSET DATA(l_newline_offset).
    rv_current_string_length  = strlen( iv_current_context ).
    DATA(lv_tmp_context) = substring( val = iv_current_context off = l_newline_offset len = ( rv_current_string_length - l_newline_offset ) ).


    lv_tmp_context = |{ lv_tmp_context }{ component_name( i_component-component ) } = '{ i_component-varvalue }'{ c_tab }|.
    "do the trick with \t and count it as 4 char spaces instead as one char
    "todo so replace it temporarly
    REPLACE ALL OCCURRENCES OF  c_tab IN  lv_tmp_context WITH |    |.
    rv_current_string_length =  strlen( lv_tmp_context ).

  ENDMETHOD.


  METHOD extract_nested_structure.
    LOOP AT ct_struc_data INTO DATA(ls_structure_data) WHERE complong CS is_component-complong.
      CHECK ls_structure_data-complong NE  is_component-complong. "we need only childs of it not it itself
      APPEND ls_structure_data TO rt_nested_struc.
      DELETE TABLE ct_struc_data  FROM ls_structure_data.
    ENDLOOP.
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


  METHOD handle_nested_structure.
    DATA:  lt_nested_structure  TYPE tpda_struc_view_it.
    lt_nested_structure = extract_nested_structure( EXPORTING is_component = is_component  CHANGING ct_struc_data  = ct_struc_data ).

    rv_current_context =  right_hand_side(
        EXPORTING
          it_struc_data            = lt_nested_structure
          iv_wrap_from_here        = iv_wrap_from_here
      ).

    rv_current_context = |{ iv_current_context }{ component_name( is_component-component ) }{ rv_current_context }{ c_tab }|.
  ENDMETHOD.


  METHOD left_hand_side.
    rv_left_hand_side_value = COND string( WHEN iv_is_type_given EQ abap_true
                                                THEN |DATA({ iv_struc_name })|
                                            ELSE iv_struc_name ).
  ENDMETHOD.


  METHOD prepare_output.
    FIELD-SYMBOLS: <s_component> TYPE tpda_struc_view.
    DATA: lv_string_main     TYPE string,
          lv_current_context TYPE string,
          lv_wrap_from_here  TYPE i VALUE 255.

    CHECK it_struc_data[] IS NOT INITIAL.

    lv_wrap_from_here = get_wrap_from_value( iv_wrap_from_here = iv_wrap_from_here  ).

    rv_content_4_display = |{ left_hand_side( iv_struc_name  = iv_struc_name ) }{ right_hand_side( it_struc_data = it_struc_data  iv_wrap_from_here = lv_wrap_from_here ) }.|.

  ENDMETHOD.


  METHOD right_hand_side.

    DATA: lv_current_context TYPE string,
          lt_struc_data      TYPE tpda_struc_view_it.
    lt_struc_data = it_struc_data.
    rv_right_hand_side_value = COND string( WHEN iv_is_type_given EQ abap_true
                                                  THEN | = VALUE \{i_type\}({ c_tab }|  "TODO implement type here
                                              ELSE | = VALUE #({ c_tab }| ).

    LOOP AT lt_struc_data ASSIGNING FIELD-SYMBOL(<s_component>).

      IF  <s_component>-varvalue CS 'Structure'.
        lv_current_context = handle_nested_structure(
                                                      EXPORTING
                                                        iv_current_context = lv_current_context
                                                        iv_wrap_from_here  = iv_wrap_from_here
                                                        is_component       = <s_component>
                                                      CHANGING
                                                        ct_struc_data = lt_struc_data ).
      ELSE.
        lv_current_context = add_component(
                                              iv_current_context    = lv_current_context
                                              iv_wrap_from_here     = iv_wrap_from_here
                                              i_component            = <s_component> ).
      ENDIF.
    ENDLOOP.

    rv_right_hand_side_value = |{ rv_right_hand_side_value }{ lv_current_context })|.
  ENDMETHOD.


  METHOD show_popup_w_content.
    DATA: lv_content_4_display TYPE string.
    lv_content_4_display  = prepare_output( iv_struc_name = iv_struc_name
                                            it_struc_data = it_struc_data ).

    cl_demo_output=>set_mode( cl_demo_output=>text_mode ). "set to text mode to be more compatible with minus signs and so on
    cl_demo_output=>display( lv_content_4_display ).
  ENDMETHOD.
  METHOD component_name.
    rv_component_name = iv_component_name.
*    rv_component_name = condense( val =  iv_component_name del = space ).
    CONDENSE rv_component_name NO-GAPS.
  ENDMETHOD.

ENDCLASS.
