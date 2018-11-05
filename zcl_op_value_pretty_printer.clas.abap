"!this class is responsible for pretty printing
"!VALUE # operator strings
CLASS zcl_op_value_pretty_printer DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    METHODS:
      format
        IMPORTING i_unformated_value_content TYPE string
        RETURNING VALUE(r_formated_content)  TYPE string.
  PROTECTED SECTION.
  PRIVATE SECTION.
    TYPES: BEGIN OF t_char_offset,
             count     TYPE i,
             offset_no TYPE i,
             char(1)   TYPE c,
           END OF t_char_offset,
           tt_char_offset TYPE STANDARD TABLE OF t_char_offset WITH DEFAULT KEY.
    DATA: steering TYPE tt_char_offset.
    METHODS:
      get_spaces RETURNING VALUE(r_spaces_string) TYPE string,
      get_offset_in_current_line
        IMPORTING
          i_content       TYPE string
        RETURNING
          VALUE(r_offset) TYPE i,
      get_offset_for_apostroph
        IMPORTING
          i_content       TYPE string
        RETURNING
          VALUE(r_offset) TYPE i,
      get_offset_for_closing_bracket
        IMPORTING
          i_content       TYPE string
        RETURNING
          VALUE(r_offset) TYPE i,
      clean_up_steering_tab
        IMPORTING
          i_last_char TYPE c.
    CONSTANTS:
      c_newline TYPE abap_char1 VALUE cl_abap_char_utilities=>cr_lf.


ENDCLASS.



CLASS zcl_op_value_pretty_printer IMPLEMENTATION.

  METHOD format.
    DATA: last_char(1) TYPE c.
    r_formated_content = i_unformated_value_content.
    DATA(reader) = NEW cl_abap_string_c_reader( str = i_unformated_value_content ).
    DATA(writer) = NEW cl_abap_string_c_writer( ).


    WHILE reader->data_available( ) = abap_true.
      DATA(char) = reader->read( 1 ).

      IF  char EQ | | AND ( last_char EQ |(| OR last_char EQ |)| OR last_char EQ |'| ).
        clean_up_steering_tab( last_char ).
        writer->write( |{ c_newline }{ get_spaces( ) }| ).
      ELSE.


        DATA(offset) = COND i(  WHEN char EQ |(| THEN get_offset_in_current_line( writer->get_result_string( ) )
                                WHEN char EQ |)| THEN get_offset_for_closing_bracket( writer->get_result_string( ) ) "need special handling here as in this case we need the offset of starting ( of it
                                WHEN last_char IS NOT INITIAL AND char EQ |'| THEN get_offset_for_apostroph( writer->get_result_string( ) )  "need special handling here as in this case we need the offset of starting component of it
                                ELSE 0 ).

        IF offset > 0.
          APPEND VALUE #( count = ( lines( steering ) + 1 )
                          offset_no = offset
                          char = char ) TO steering.
        ENDIF.

        writer->write( char ).
      ENDIF.

      last_char = char.
    ENDWHILE.

    reader->close( ).
    writer->close( ).

    r_formated_content = writer->get_result_string( ).
  ENDMETHOD.

  METHOD get_spaces.
    DATA(spaces_no) = steering[ lines( steering ) ]-offset_no.
    DO spaces_no TIMES.
      r_spaces_string = |{ r_spaces_string } |.
    ENDDO.
  ENDMETHOD.


  METHOD get_offset_in_current_line.
    SPLIT i_content  AT c_newline INTO TABLE DATA(lt_split).
    DATA(last_line) = lt_split[ lines( lt_split ) ].
    r_offset = strlen( last_line ) + 1.
  ENDMETHOD.


  METHOD get_offset_for_apostroph.
    SPLIT i_content  AT c_newline INTO TABLE DATA(lt_split).
    DATA(last_line) = lt_split[ lines( lt_split ) ].

    DATA(regex_matcher) = cl_abap_matcher=>create(  pattern = |\\S|
                                                    text    = last_line    ).

    regex_matcher->find_next( ).
    r_offset = regex_matcher->get_offset( ).

  ENDMETHOD.


  METHOD get_offset_for_closing_bracket.
    "find offset of corresponding opening bracket
    "therefor we need to loop backwards in steering table

    DATA(x_lines) = lines( steering ).

    DO x_lines TIMES.
      DATA(line) = steering[ x_lines ].
      IF line-char EQ |(|.
        r_offset = line-offset_no.
        EXIT.
      ELSE.
        IF x_lines = 1.
          EXIT.
        ELSE.
          x_lines = x_lines - 1.
        ENDIF.
      ENDIF.
    ENDDO.

  ENDMETHOD.


  METHOD clean_up_steering_tab.
    CHECK i_last_char EQ |)|.
    "a formating block is complete
    "we need to clean up our steering data and remove infos about this complete formating block
    DATA(x_lines) = lines( steering ).

    DO x_lines TIMES.
      DATA(line) = steering[ x_lines ].
      DELETE steering INDEX x_lines.
      IF line-char EQ |(|.
        EXIT.
      ENDIF.

      IF x_lines = 1.
        EXIT.
      ELSE.
        x_lines = x_lines - 1.
      ENDIF.

    ENDDO.

  ENDMETHOD.

ENDCLASS.
