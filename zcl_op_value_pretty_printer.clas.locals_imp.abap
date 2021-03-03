*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations

class lcx_no_match IMPLEMENTATION.
  METHOD constructor.
    super->constructor(
      EXPORTING
        textid   = textid
        previous = previous
    ).
    me->regex = regex.
    me->offset = offset.
    me->text_excerpt_offset = text_excerpt_offset.
    me->text_excerpt_length = text_excerpt_length.
    me->text_excerpt = text_excerpt.
  ENDMETHOD.
endclass.
