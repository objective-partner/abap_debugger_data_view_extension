CLASS zcl_op_value_pretty_printer_2 DEFINITION
  PUBLIC
  CREATE PRIVATE
  GLOBAL FRIENDS zcl_op_pretty_printer_factory. "needed for instantiation

  PUBLIC SECTION.
    INTERFACES zif_op_value_pretty_printer.
    METHODS constructor
      IMPORTING
        indent_size     TYPE i DEFAULT 2
        max_line_length TYPE i DEFAULT 128.

  PROTECTED SECTION.
  PRIVATE SECTION.
    TYPES: BEGIN OF ty_block,
             id             TYPE i,
             group_id       TYPE i,
             char(1)        TYPE c,
             level_of_depth TYPE i,
             line_index     TYPE i,
             position       TYPE i,     "not necessary
           END OF ty_block,
           ty_block_table TYPE SORTED TABLE OF ty_block WITH UNIQUE KEY id,
           ty_lines       TYPE TABLE OF string WITH DEFAULT KEY.

    DATA m_indent_size TYPE i.
    DATA m_max_line_length TYPE i.
    DATA m_reader TYPE REF TO cl_abap_string_c_reader.
    DATA m_blocks TYPE ty_block_table.
    DATA m_line TYPE string.
    DATA m_lines TYPE ty_lines.

    CONSTANTS c_newline TYPE abap_char1 VALUE cl_abap_char_utilities=>cr_lf.
    CONSTANTS c_opening_bracket TYPE c VALUE '('.
    CONSTANTS c_closing_bracket TYPE c VALUE ')'.
    CONSTANTS c_enclosure TYPE c VALUE ''''.
    CONSTANTS c_enclosure_escape TYPE c VALUE ''''.

    METHODS get_lines_as_string
      RETURNING
        VALUE(r_string) TYPE string.

    "! Processes next char read by reader
    METHODS handle_char
      IMPORTING
        i_char TYPE string.

    "! Reads component value that is between enclosure characters (respecting enclosure escapes).
    "! Example: Returns "It''s easy (isn''t it?)" from " 'It''s easy (isn''t it?)' ".
    METHODS read_value_between_enclosures
      RETURNING
        VALUE(r_value) TYPE string.

    METHODS split_value_at_max_line_length
      IMPORTING
        i_length       TYPE i
        i_value        TYPE string
      RETURNING
        VALUE(r_lines) TYPE ty_lines.

    METHODS add_to_block_control
      IMPORTING
        i_char         TYPE string
        i_line_index   TYPE i
        i_position     TYPE i
      RETURNING
        VALUE(r_block) TYPE ty_block.

    "! Calculates number of spaces for the next line.
    "! Previous line has to be appended already to m_lines and block control must exist.
    METHODS calculate_indent
      RETURNING
        VALUE(r_spaces) TYPE string.

    METHODS calculate_level_of_depth
      IMPORTING
        i_char                  TYPE string
      RETURNING
        VALUE(r_level_of_depth) TYPE i.

    METHODS calculate_group_id
      IMPORTING
        i_char            TYPE string
      RETURNING
        VALUE(r_group_id) TYPE i.

    METHODS append_line.

    METHODS add_indents.
ENDCLASS.



CLASS zcl_op_value_pretty_printer_2 IMPLEMENTATION.


  METHOD add_indents.
*    TYPES: BEGIN OF ty_group2lines,
*             group_id         TYPE ty_block-group_id,
*             line_index_range TYPE RANGE OF ty_block-line_index,
*           END OF ty_group2lines.
*    DATA group2lines TYPE SORTED TABLE OF ty_group2lines WITH UNIQUE KEY group_id.
*    LOOP AT m_blocks INTO DATA(block) GROUP BY block-group_id ASCENDING INTO DATA(groups).
*      LOOP AT GROUP groups INTO DATA(group).
*        TRY.
*            ASSIGN group2lines[ group_id = group-group_id ] TO FIELD-SYMBOL(<group2lines>).
*            <group2lines>-line_index_range[ 1 ]-low = COND #(
*              WHEN <group2lines>-line_index_range[ 1 ]-low < group-line_index
*              THEN <group2lines>-line_index_range[ 1 ]-low
*              ELSE group-line_index ).
*            <group2lines>-line_index_range[ 1 ]-high = COND #(
*              WHEN <group2lines>-line_index_range[ 1 ]-high > group-line_index
*              THEN 0
*              ELSE 0
*            ).
*          CATCH cx_sy_move_cast_error.
*            group2lines = VALUE #( BASE group2lines (
*              group_id = group-group_id
*              line_index_range = VALUE #( ( sign = 'I' option = 'EQ' low = group-line_index ) )
*            ) ).
*        ENDTRY.
*      ENDLOOP.
*    ENDLOOP.

    LOOP AT m_lines ASSIGNING FIELD-SYMBOL(<line>).
      DATA(line_index) = sy-tabix.
      IF line_exists( m_blocks[ line_index = line_index ] ).
        DATA(level_of_depth) = m_blocks[ line_index = line_index ]-level_of_depth.
        DATA(num_spaces) = ( m_indent_size * ( level_of_depth - 1 ) ).
      ELSE.
        num_spaces = ( m_indent_size * ( level_of_depth ) ).
      ENDIF.
*      DATA(line_index_start) = sy-tabix.
*      DATA(line_index) = sy-tabix.
*      WHILE line_index > 0.
*        "find corresponding block control record
*        IF line_exists( m_blocks[ line = line_index ] ).
*          DATA(level_of_depth) = m_blocks[ line = line_index ]-level_of_depth.
*          DATA(num_spaces) = COND i( WHEN line_index = line_index_start
*            THEN ( m_indent_size * ( level_of_depth - 1 ) )
*            ELSE ( m_indent_size * level_of_depth )
*          ).
*          DO num_spaces TIMES.
*            <line> = | { <line> }|.
*          ENDDO.
*          EXIT.
*        ENDIF.
*        line_index = line_index - 1.
*      ENDWHILE.
    ENDLOOP.
  ENDMETHOD.


  METHOD add_to_block_control.
    DATA block LIKE LINE OF m_blocks.

    "Example:           Line
    "D = VALUE #(          1
    "COL1 = '(1)'          2
    "COL2 = VALUE #(       3
    "COL1 = '1'            4
    "COL2 = '2'            5
    "COL3 = 'It''s easy'   6
    ")                     7
    "COL3 = '3'            8
    "COL4 = VALUE #(       9
    "COL1 = '1'           10
    "COL2 = '2'           11
    ")                    12
    ").                   13
    "123456789012345678 Position

    "Blocks:
    "|ID |GR_ID|CHAR|LEVEL|LINE|POS
    "|1  |1    |(   |1    |1   |12
    "|2  |2    |(   |2    |3   |15
    "|3  |2    |)   |2    |7   |1
    "|4  |3    |(   |2    |9   |15
    "|5  |3    |)   |2    |12  |1
    "|6  |1    |)   |1    |13  |1

    CHECK i_char = c_opening_bracket
       OR i_char = c_closing_bracket.

    block-id = lines( m_blocks ) + 1.
    block-char = i_char.
    block-line_index = i_line_index.
    block-position = i_position.
    block-level_of_depth = calculate_level_of_depth( i_char ).
    block-group_id = calculate_group_id( i_char ).

    m_blocks = VALUE #( BASE m_blocks ( block ) ).
  ENDMETHOD.


  METHOD append_line.
    APPEND m_line TO m_lines.
    CLEAR m_line.
  ENDMETHOD.


  METHOD calculate_group_id.
    DATA excluded_group_ids TYPE TABLE OF ty_block-group_id.
    DATA(index) = lines( m_blocks ).

    "each opening bracket starts a new group
    IF i_char = c_opening_bracket.
      r_group_id = COND #( WHEN index > 0
        THEN m_blocks[ index ]-group_id + 1
        ELSE 1 ).
      RETURN.
    ENDIF.

    "find matching opening bracket for each closing bracket
    "and assign group id of corresponding opening bracket
    IF i_char = c_closing_bracket.
      WHILE index > 0.
        ASSIGN m_blocks[ index ] TO FIELD-SYMBOL(<block>).
        IF <block>-char = c_closing_bracket.
          "group is already closed so exclude its group_id
          excluded_group_ids = VALUE #( BASE excluded_group_ids ( <block>-group_id ) ).
        ELSEIF <block>-char = c_opening_bracket
        AND NOT line_exists( excluded_group_ids[ table_line = <block>-group_id ] ).
          "opening bracket found which is not closed yet
          r_group_id = <block>-group_id.
          RETURN.
        ENDIF.
        index = index - 1.
      ENDWHILE.
    ENDIF.
  ENDMETHOD.


  METHOD calculate_indent.
*    DATA level TYPE ty_block-level_of_depth.
*    TRY.
*        IF lines( m_lines ) < 1.
*          RETURN.
*        ENDIF.
*        DATA(last_line) = m_lines[ lines( m_lines ) ].
*        DATA(last_char_of_last_line) = substring( val = last_line off = strlen( last_line ) - 1 ).
*
*        CASE last_char_of_last_line.
*          WHEN c_enclosure OR c_opening_bracket.
*            level = m_blocks[ lines( m_blocks ) ]-level_of_depth.
*          WHEN c_closing_bracket.
*            level = m_blocks[ lines( m_blocks ) ]-level_of_depth - 1.
*        ENDCASE.
*
*        DATA(lv_num_spaces) = level * m_indent_size.
*        DO lv_num_spaces TIMES.
*          r_spaces = |{ r_spaces } |.
*        ENDDO.
*      CATCH cx_sy_itab_line_not_found.
*    ENDTRY.
  ENDMETHOD.


  METHOD calculate_level_of_depth.
    DATA(index) = lines( m_blocks ).
    IF index = 0.               "first entry
      r_level_of_depth = 1.
      RETURN.
    ENDIF.
    DATA(previous_block) = m_blocks[ index ].
    r_level_of_depth = COND #(
      WHEN i_char = c_opening_bracket AND previous_block-char = c_closing_bracket
        OR i_char = c_closing_bracket AND previous_block-char = c_opening_bracket
        THEN previous_block-level_of_depth
      WHEN i_char = c_opening_bracket AND previous_block-char = c_opening_bracket
        THEN previous_block-level_of_depth + 1
      WHEN i_char = c_closing_bracket AND previous_block-char = c_closing_bracket
        THEN previous_block-level_of_depth - 1
    ).
  ENDMETHOD.


  METHOD constructor.
    m_indent_size = indent_size.
    m_max_line_length = COND #(
      WHEN max_line_length > 0 AND max_line_length <= 255
      THEN max_line_length
      ELSE 128
    ).
  ENDMETHOD.


  METHOD get_lines_as_string.
    DATA(writer) = NEW cl_abap_string_c_writer( ).

    LOOP AT m_lines ASSIGNING FIELD-SYMBOL(<line>).
      IF sy-tabix > 1.
        writer->write( |{ c_newline }| ).
      ENDIF.
      writer->write( <line> ).
    ENDLOOP.

    writer->close( ).
    r_string = writer->get_result_string( ).
  ENDMETHOD.


  METHOD handle_char.
    "Skip blanks between brackets and components
    IF m_line CO space AND i_char CO space.       "eq space does not work
      RETURN.
    ENDIF.

    IF i_char = c_enclosure.
      DATA(value) = read_value_between_enclosures( ).
      IF strlen( m_line ) + strlen( value ) < m_max_line_length.
        m_line = m_line && value.
        append_line( ).
      ELSE.
        "TODO line breaks
      ENDIF.
      RETURN.
    ENDIF.

    m_line = m_line && i_char.

    IF i_char = c_opening_bracket
    OR i_char = c_closing_bracket.
      append_line( ).
      add_to_block_control(
        i_char = i_char
        i_line_index = lines( m_lines )
        i_position = strlen( m_line ) ).
    ENDIF.
  ENDMETHOD.


  METHOD read_value_between_enclosures.
    DATA char TYPE string.
    TRY.
        DATA(value) = | { c_enclosure }|.
        DO.
          char = m_reader->read( 1 ).
          value = value && char.
          "^[ ]['](.*)['][ ]$ extracts value only
          "^[ ](['].*['])[ ]$ extracts value surrounded by enclosures
          DATA(matcher) = cl_abap_matcher=>create(
           EXPORTING
             pattern = |^[ ]([{ c_enclosure }].*[{ c_enclosure }])[ ]$|
             text    = value
          ).
          IF matcher->match( ) = abap_true.
            r_value = matcher->get_submatch( 1 ).
            RETURN.
          ENDIF.
        ENDDO.
      CATCH cx_sy_regex.
      CATCH cx_sy_matcher.
    ENDTRY.
  ENDMETHOD.


  METHOD split_value_at_max_line_length.
    IF i_length + strlen( i_value ) <= m_max_line_length.
      RETURN.
    ENDIF.

  ENDMETHOD.


  METHOD zif_op_value_pretty_printer~format.
    DATA char TYPE string.     "work throughout with strings as spaces are preserved
    m_reader = NEW cl_abap_string_c_reader( str = i_unformated_value_content ).

    WHILE m_reader->data_available( ) = abap_true.
      char = m_reader->read( 1 ).
      handle_char( char ).
    ENDWHILE.
    m_reader->close( ).
    "VALUE has to end with a dot.     #TODO: unformatted value content should contain the dot.
    ASSIGN m_lines[ lines( m_lines ) ] TO FIELD-SYMBOL(<last_line>).
    <last_line> = |{ <last_line> }.|.

    add_indents( ).
    r_formated_content = get_lines_as_string( ).
  ENDMETHOD.
ENDCLASS.
