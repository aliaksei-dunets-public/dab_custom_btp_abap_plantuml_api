CLASS zdab_cx_uml_parser DEFINITION
  PUBLIC
  INHERITING FROM cx_static_check
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES if_abap_behv_message .

*    CONSTANTS:
*
*      gc_msgid TYPE symsgid VALUE 'ZDAB_CUST_COMMON'  ##NO_TEXT,
*
*      " Mandatory fields is not filled
*      BEGIN OF missed_manadatory_field,
*        msgid TYPE symsgid VALUE 'ZDAB_CUST_COMMON',
*        msgno TYPE symsgno VALUE '010',
*        attr1 TYPE scx_attrname VALUE '',
*        attr2 TYPE scx_attrname VALUE '',
*        attr3 TYPE scx_attrname VALUE '',
*        attr4 TYPE scx_attrname VALUE '',
*      END OF missed_manadatory_field,
*
*      " Class Handler &1 is invalid
*      BEGIN OF class_handler_invalid,
*        msgid TYPE symsgid VALUE 'ZDAB_CUST_COMMON',
*        msgno TYPE symsgno VALUE '011',
*        attr1 TYPE scx_attrname VALUE 'MV_ATTR1',
*        attr2 TYPE scx_attrname VALUE '',
*        attr3 TYPE scx_attrname VALUE '',
*        attr4 TYPE scx_attrname VALUE '',
*      END OF class_handler_invalid.

    METHODS constructor
      IMPORTING
        !textid   LIKE if_t100_message=>t100key OPTIONAL
        !previous LIKE previous OPTIONAL
        !v1       TYPE string OPTIONAL
        !v2       TYPE string OPTIONAL
        !v3       TYPE string OPTIONAL
        !v4       TYPE string OPTIONAL
        !severity TYPE if_abap_behv_message=>t_severity OPTIONAL.

    DATA:
      mv_attr1 TYPE string,
      mv_attr2 TYPE string,
      mv_attr3 TYPE string,
      mv_attr4 TYPE string.

  PROTECTED SECTION.
  PRIVATE SECTION.

ENDCLASS.

CLASS zdab_cx_uml_parser IMPLEMENTATION.

  METHOD constructor ##ADT_SUPPRESS_GENERATION.
    CALL METHOD super->constructor
      EXPORTING
        previous = previous.

    me->mv_attr1 = v1.
    me->mv_attr2 = v2.
    me->mv_attr3 = v3.
    me->mv_attr4 = v4.

    if_abap_behv_message~m_severity = severity.

    CLEAR me->textid.
    IF textid IS INITIAL.
      if_t100_message~t100key = if_t100_message=>default_textid.
    ELSE.
      if_t100_message~t100key = textid.
    ENDIF.
  ENDMETHOD.
ENDCLASS.
