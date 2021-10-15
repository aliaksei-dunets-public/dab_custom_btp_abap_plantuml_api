CLASS zdab_cl_uml_class_api DEFINITION
  INHERITING FROM zdab_cl_uml_element_base_api
  PUBLIC
  CREATE PUBLIC .

  PUBLIC SECTION.

    METHODS:
      get_child_uml_code REDEFINITION,

      set_class_type IMPORTING iv_class_type TYPE string,
      get_class_type RETURNING VALUE(rv_class_type) TYPE string,
      set_super_class IMPORTING iv_class_name TYPE string,
      set_interface IMPORTING iv_interface_name TYPE string.

  PROTECTED SECTION.
    DATA:
      mv_class_type  TYPE string,
      mo_super_class TYPE REF TO zdab_if_uml_element_api.

    METHODS:
      parse_uml_code REDEFINITION.

  PRIVATE SECTION.
ENDCLASS.


CLASS zdab_cl_uml_class_api IMPLEMENTATION.


  METHOD set_interface.

    zdab_cl_uml_element_relate_api=>get_instance( )->add_relation( VALUE #( source_name   = mv_element_name
                                                                            target_name   = iv_interface_name
                                                                            target_type   = zdab_if_uml_element_api=>gc_class_type-interface
                                                                            relation_type = zdab_if_uml_element_api=>gc_relation_type-implementation ) ).

*    zdab_cl_uml_element_relate_api=>get_instance( )->add_relations( it_relation = VALUE #( ( source_element = me
*                                                                                             target_element = io_interface
*                                                                                             relation_type  = zdab_if_uml_element_api=>gc_relation_type-implementation ) ) ).

  ENDMETHOD.


  METHOD get_class_type.
    rv_class_type = mv_class_type.
  ENDMETHOD.


  METHOD parse_uml_code.
    "<mv_class_type> <mv_element_name> {
    "<mt_child_elements>
    "}

    " Add Header of the element
    rv_uml_code = |{ mv_class_type } { to_lower( mv_element_name ) } | &&
                  |{ gc_curly_brace_open } { gc_new_line }|.

    " Add Body of the element
    rv_uml_code = |{ rv_uml_code } { get_child_uml_code( ) }|.

    " Add Footer of the element
    rv_uml_code = |{ rv_uml_code } { gc_curly_brace_closed } { gc_new_line }|.

  ENDMETHOD.


  METHOD set_class_type.
    mv_class_type = iv_class_type.
  ENDMETHOD.


  METHOD set_super_class.

    zdab_cl_uml_element_relate_api=>get_instance( )->add_relation( VALUE #( source_name   = mv_element_name
                                                                            target_name   = iv_class_name
                                                                            target_type   = zdab_if_uml_element_api=>gc_class_type-class
                                                                            relation_type = zdab_if_uml_element_api=>gc_relation_type-extension ) ).


*    mo_super_class = io_super_class.
*
*    zdab_cl_uml_element_relate_api=>get_instance( )->add_relations( it_relation = VALUE #( ( source_element = me
*                                                                                             target_element = mo_super_class
*                                                                                             relation_type  = zdab_if_uml_element_api=>gc_relation_type-extension ) ) ).

  ENDMETHOD.


  METHOD get_child_uml_code.

    DATA:
      lv_class_types     TYPE string,
      lv_class_constants TYPE string,
      lv_class_alias     TYPE string,
      lv_class_fields    TYPE string,
      lv_class_body      TYPE string.

    LOOP AT mt_child_elements INTO DATA(lo_child_element).

      CASE cl_abap_classdescr=>get_class_name( lo_child_element ).
          " CLASS TYPES
        WHEN '\CLASS=ZDAB_CL_UML_CLASS_TYPES_API'   ##NO_TEXT.
          lv_class_types = |{ lv_class_types } { lo_child_element->get_uml_code( ) }|.

          " CLASS CONSTANTS
        WHEN '\CLASS=ZDAB_CL_UML_CLASS_CONSTANT_API' ##NO_TEXT.
          lv_class_constants = |{ lv_class_constants } { lo_child_element->get_uml_code( ) }|.

          " CLASS ALIAS
        WHEN '\CLASS=ZDAB_CL_UML_CLASS_ALIAS_API' ##NO_TEXT.
          lv_class_alias = |{ lv_class_alias } { lo_child_element->get_uml_code( ) }|.

          " CLASS ATTRIBUTES
        WHEN '\CLASS=ZDAB_CL_UML_CLASS_FIELD_API' ##NO_TEXT.
          lv_class_fields = |{ lv_class_fields } { lo_child_element->get_uml_code( ) }|.

          " CLASS METHODS
        WHEN OTHERS.
          lv_class_body = |{ lv_class_body } { lo_child_element->get_uml_code( ) }|.
      ENDCASE.

    ENDLOOP.

    IF lv_class_types IS NOT INITIAL.
      ".. TYPES ..
      "<types>
      "..
      lv_class_types = |.. TYPES ..{ gc_new_line } { lv_class_types } ..{ gc_new_line }|  ##NO_TEXT.
    ENDIF.

    IF lv_class_constants IS NOT INITIAL.
      ".. CONSTANTS ..
      "<constants>
      "..
      lv_class_constants = |.. CONSTANTS ..{ gc_new_line } { lv_class_constants } ..{ gc_new_line }|  ##NO_TEXT.
    ENDIF.

    IF lv_class_alias IS NOT INITIAL.
      ".. CONSTANTS ..
      "<constants>
      "..
      lv_class_alias = |.. ALIAS ..{ gc_new_line } { lv_class_alias } ..{ gc_new_line }|  ##NO_TEXT.
    ENDIF.

    IF  lv_class_fields IS NOT INITIAL
    AND ( lv_class_constants IS NOT INITIAL
    OR    lv_class_alias IS NOT INITIAL ).
      "__
      "<classFields>
      lv_class_fields = |__{ gc_new_line } { lv_class_fields }|.
    ENDIF.

    rv_child_uml_code = |{ lv_class_types }| &&
                        |{ lv_class_constants }| &&
                        |{ lv_class_alias }| &&
                        |{ lv_class_fields }| &&
                        |__{ gc_new_line } { lv_class_body }|.

  ENDMETHOD.
ENDCLASS.
