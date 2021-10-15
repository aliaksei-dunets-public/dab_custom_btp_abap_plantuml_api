CLASS zdab_cl_uml_element_base_api DEFINITION
  PUBLIC
  ABSTRACT
  CREATE PUBLIC .

  PUBLIC SECTION.

    CONSTANTS:
      gc_new_line           TYPE abap_char1 VALUE zdab_if_uml_element_api=>gc_new_line,
      gc_curly_brace_open   TYPE string VALUE zdab_if_uml_element_api=>gc_curly_brace_open,
      gc_curly_brace_closed TYPE string VALUE zdab_if_uml_element_api=>gc_curly_brace_closed.

    INTERFACES zdab_if_uml_element_api.

    ALIASES:
      set_name_element      FOR zdab_if_uml_element_api~set_name_element,
      get_name_element      FOR zdab_if_uml_element_api~get_name_element,
      set_parent_element    FOR zdab_if_uml_element_api~set_parent_element,
      get_parent_element    FOR zdab_if_uml_element_api~get_parent_element,
      add_child_element     FOR zdab_if_uml_element_api~add_child_element,
      get_child_elements    FOR zdab_if_uml_element_api~get_child_elements,
      get_child_uml_code    FOR zdab_if_uml_element_api~get_child_uml_code,
      get_uml_code          FOR zdab_if_uml_element_api~get_uml_code.

    METHODS:
      constructor
        IMPORTING
          iv_element_name TYPE string.

  PROTECTED SECTION.

    DATA:
      mv_element_name   TYPE string,
      mo_parent_element TYPE REF TO zdab_if_uml_element_api,
      mt_child_elements TYPE STANDARD TABLE OF REF TO zdab_if_uml_element_api.

    METHODS:
      parse_uml_code ABSTRACT
        RETURNING VALUE(rv_uml_code) TYPE string.

  PRIVATE SECTION.

ENDCLASS.



CLASS ZDAB_CL_UML_ELEMENT_BASE_API IMPLEMENTATION.


  METHOD zdab_if_uml_element_api~set_parent_element.
    mo_parent_element = io_parent_element.
  ENDMETHOD.


  METHOD zdab_if_uml_element_api~set_name_element.
    mv_element_name = iv_element_name.
  ENDMETHOD.


  METHOD zdab_if_uml_element_api~get_name_element.
    rv_element_name = mv_element_name.
  ENDMETHOD.


  METHOD zdab_if_uml_element_api~get_child_uml_code.
    LOOP AT mt_child_elements INTO DATA(lo_child_element).
      rv_child_uml_code = |{ rv_child_uml_code } { lo_child_element->get_uml_code( ) }|.
    ENDLOOP.
  ENDMETHOD.


  METHOD zdab_if_uml_element_api~add_child_element.
    io_child_element->set_parent_element( io_parent_element = me ).
    APPEND io_child_element TO mt_child_elements.
  ENDMETHOD.


  METHOD zdab_if_uml_element_api~get_child_elements.
    rt_child_elements = mt_child_elements.
  ENDMETHOD.


  METHOD zdab_if_uml_element_api~get_parent_element.
    ro_parent_element = mo_parent_element.
  ENDMETHOD.


  METHOD zdab_if_uml_element_api~get_uml_code.
    rv_uml_code = parse_uml_code( ).
  ENDMETHOD.


  METHOD constructor.
    set_name_element( iv_element_name ).
  ENDMETHOD.
ENDCLASS.
