"! Initially created by Andreas Kopp
"! This class is a pretty printer for abap value # expression
"! It consider a max line lenght of 128 chars
CLASS zcl_max_line_length_pp DEFINITION
  PUBLIC
  CREATE PRIVATE
  GLOBAL FRIENDS zcl_op_pretty_printer_factory. "needed for instantiation

  PUBLIC SECTION.
    INTERFACES zif_op_value_pretty_printer.

    ALIASES: format FOR zif_op_value_pretty_printer~format.

    "! @parameter indent_size | Number of spaces to begin each line with
    "! @parameter max_line_length | Maximum number of characters for each line. Value has
    "! to be between 20 and 255 characters (maximum of ABAP editor).
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
             "block is completed when '(' and ')' for the same group_id exists
             is_completed   TYPE abap_bool,
           END OF ty_block,
           ty_block_table TYPE SORTED TABLE OF ty_block WITH UNIQUE KEY id,
           ty_lines       TYPE TABLE OF        string WITH DEFAULT KEY.

    DATA:
      m_indent_size     TYPE        i,
      m_max_line_length TYPE        i,
      m_reader          TYPE REF TO cl_abap_string_c_reader,
      m_blocks          TYPE        ty_block_table,
      m_line            TYPE        string,
      m_lines           TYPE        ty_lines.

    CONSTANTS:
      c_newline          TYPE abap_char1 VALUE cl_abap_char_utilities=>cr_lf,
      c_opening_bracket  TYPE c VALUE '(',
      c_closing_bracket  TYPE c VALUE ')',
      c_enclosure        TYPE c VALUE '''',
      c_enclosure_escape TYPE c VALUE '''',
      c_concatenate      TYPE c VALUE '&'.

    METHODS:
      get_lines_as_string
        RETURNING
          VALUE(r_string) TYPE string,

      "! Processes next char read by reader
      handle_char
        IMPORTING
          i_char TYPE string,

      "! Reads component value that is between enclosure characters (respecting enclosure escapes).
      "! Example: Returns "It''s easy (isn''t it?)" from "[COL =] 'It''s easy (isn''t it?)' ".
      read_value_between_enclosures
        RETURNING
          VALUE(r_value) TYPE string,

      "! Splits value into several lines. The length of each line is less than the maximum line size.
      "! The remaining characters of the first line can be used to fill up the first line.
      "!
      "! @parameter i_length_of_first_line | characters already used for the first line
      "! @parameter i_value | value to split up
      "! @parameter i_indent_size | number of spaces to begin each line beginning from the second line
      split_value_at_max_line_length
        IMPORTING
          i_length_of_first_line TYPE i
          i_value                TYPE string
          i_indent_size          TYPE i OPTIONAL
        RETURNING
          VALUE(r_parts)         TYPE ty_lines,

      add_to_block_control
        IMPORTING
          i_char         TYPE string
          i_line_index   TYPE i
        RETURNING
          VALUE(r_block) TYPE ty_block,

      "! Calculates number of spaces for the next line.
      "! Previous line has to be appended already to m_lines and block control must exist.
      calculate_indent
        RETURNING
          VALUE(r_spaces) TYPE string,

      calculate_level_of_depth
        IMPORTING
          i_char                  TYPE string
        RETURNING
          VALUE(r_level_of_depth) TYPE i,

      calculate_group_id
        IMPORTING
          i_char            TYPE string
        RETURNING
          VALUE(r_group_id) TYPE i,

      append_line,
      update_indent_of_last_line,
      clear_members.
ENDCLASS.



CLASS zcl_max_line_length_pp IMPLEMENTATION.


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

    "Blocks:
    "|ID |GROUP|CHAR|LEVEL|LINE
    "|1  |1    |(   |1    |1
    "|2  |2    |(   |2    |3
    "|3  |2    |)   |2    |7
    "|4  |3    |(   |2    |9
    "|5  |3    |)   |2    |12
    "|6  |1    |)   |1    |13

    "Blocks that belong together have the same group id.
    "Each block starts with '(' and ends with ')'.
    CHECK i_char = c_opening_bracket
       OR i_char = c_closing_bracket.

    block-id = lines( m_blocks ) + 1.
    block-char = i_char.
    block-line_index = i_line_index.
    block-level_of_depth = calculate_level_of_depth( i_char ).
    block-group_id = calculate_group_id( i_char ).

    "With the closing bracket, a group of two blocks (with the same group id) is complete.
    IF i_char = c_closing_bracket.
      block-is_completed = abap_true.
      ASSIGN m_blocks[ group_id = block-group_id ] TO FIELD-SYMBOL(<opening_block>).
      IF sy-subrc = 0.
        <opening_block>-is_completed = abap_true.
      ENDIF.
    ENDIF.

    APPEND block TO m_blocks.
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
          APPEND <block>-group_id TO excluded_group_ids.
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
    DATA level TYPE ty_block-level_of_depth.
    IF lines( m_lines ) < 1.
      RETURN.
    ENDIF.
    DATA(last_line) = m_lines[ lines( m_lines ) ].
    DATA(last_char_of_last_line) = substring( val = last_line off = strlen( last_line ) - 1 ).

    CASE last_char_of_last_line.
      WHEN c_opening_bracket OR c_closing_bracket.
        level = m_blocks[ lines( m_blocks ) ]-level_of_depth - 1.
      WHEN c_enclosure OR c_concatenate.
        "find latest block that is not completed
        DATA(block_index) = lines( m_blocks ).
        WHILE block_index > 0.
          IF m_blocks[ block_index ]-is_completed = abap_false.
            level = m_blocks[ block_index ]-level_of_depth.
            EXIT.
          ENDIF.
          block_index = block_index - 1.
        ENDWHILE.
    ENDCASE.

    DATA(lv_num_spaces) = level * m_indent_size.
    DO lv_num_spaces TIMES.
      r_spaces = |{ r_spaces } |.
    ENDDO.
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
      WHEN max_line_length >= 20 AND max_line_length <= 255
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
    FIELD-SYMBOLS <last_line> LIKE LINE OF m_lines.

    "Skip blanks between brackets and components
    IF m_line CO space AND i_char CO space.       "EQ space does not work
      RETURN.
    ENDIF.

    IF i_char = c_enclosure.
      DATA(value) = read_value_between_enclosures( ).
      DATA(parts) = split_value_at_max_line_length(
        EXPORTING
          i_length_of_first_line = strlen( m_line )         "to fill up current line
          i_value  = value
          i_indent_size = strlen( calculate_indent( ) )     "indent size needed for split calculation
      ).
      LOOP AT parts INTO DATA(part).
        m_line = m_line && part.
        append_line( ).
        update_indent_of_last_line( ).
      ENDLOOP.
      RETURN.
    ENDIF.

    m_line = m_line && i_char.

    IF i_char = c_opening_bracket
    OR i_char = c_closing_bracket.
      append_line( ).                          "append line first
      add_to_block_control(                    "block control expects that line is appended
        i_char = i_char
        i_line_index = lines( m_lines ) ).
      update_indent_of_last_line( ).           "indent calculation requires block control
    ENDIF.

  ENDMETHOD.


  METHOD read_value_between_enclosures.
    DATA char TYPE string.
    DATA(value) = | { c_enclosure }|.
    DO.
      char = m_reader->read( 1 ).
      value = value && char.
      "^[ ](['].*['])[ ]$ extracts value surrounded by enclosures
      "^[ ]['](.*)['][ ]$ extracts value only
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
  ENDMETHOD.


  METHOD split_value_at_max_line_length.
    "#TODO Don't break line between escape and enclosure.
    DATA part TYPE string.
    "if max length is not exceeded, just return input value.
    IF i_length_of_first_line + strlen( i_value ) <= m_max_line_length.
      APPEND i_value TO r_parts.
      RETURN.
    ENDIF.

    DATA(lo_reader) = NEW cl_abap_string_c_reader( str = i_value ).
    DATA(max_length) = m_max_line_length - i_length_of_first_line.  "first line is shorter
    WHILE lo_reader->data_available( ) = abap_true.
      IF strlen( part ) = ( max_length - i_indent_size - 4 ).  "4 chars for closing enclosure, space and &&
        APPEND |{ part }{ c_enclosure } { c_concatenate }{ c_concatenate }| TO r_parts.
        part = |{ c_enclosure }|.           "starting with indent and a opening enclosure
        DO i_indent_size TIMES.
          part = | { part }|.
        ENDDO.
        max_length = m_max_line_length.     "full line length beginning at 2nd line
      ENDIF.
      part = part && lo_reader->read( 1 ).
    ENDWHILE.
    lo_reader->close( ).
    IF part IS NOT INITIAL.
      APPEND |{ part }| TO r_parts.
    ENDIF.
  ENDMETHOD.


  METHOD update_indent_of_last_line.
    ASSIGN m_lines[ lines( m_lines ) ] TO FIELD-SYMBOL(<last_line>).
    CONDENSE <last_line>.
    <last_line> = |{ calculate_indent( ) }{ <last_line> }|.
  ENDMETHOD.


  METHOD zif_op_value_pretty_printer~format.

    DATA char TYPE string.     "work throughout with strings as spaces are preserved

    clear_members( ).

    m_reader = NEW cl_abap_string_c_reader( str = i_unformated_value_content ).

    WHILE m_reader->data_available( ) = abap_true.
      char = m_reader->read( 1 ).
      handle_char( char ).
    ENDWHILE.
    m_reader->close( ).

    r_formated_content = get_lines_as_string( ).
  ENDMETHOD.

  METHOD clear_members.
    CLEAR: m_reader, m_blocks, m_line, m_lines.
  ENDMETHOD.

ENDCLASS.
