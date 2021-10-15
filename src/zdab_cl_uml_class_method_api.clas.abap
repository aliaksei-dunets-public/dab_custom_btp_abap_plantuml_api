CLASS zdab_cl_uml_class_method_api DEFINITION
  INHERITING FROM zdab_cl_uml_element_base_api
  PUBLIC
  CREATE PUBLIC .

  PUBLIC SECTION.

    METHODS:
      set_visibility
        IMPORTING iv_visibility TYPE string,
      get_visibility
        RETURNING VALUE(rv_visibility) TYPE string,

      set_abstract_indicator
        IMPORTING iv_abstract TYPE abap_bool,
      get_abstract_indicator
        RETURNING VALUE(rv_abstract) TYPE abap_bool,

      set_static_indicator
        IMPORTING iv_static TYPE abap_bool,
      get_static_indicator
        RETURNING VALUE(rv_static) TYPE abap_bool.

  PROTECTED SECTION.
    DATA:
      mv_visibility TYPE string,
      mv_field_type TYPE string,
      mv_abstract   TYPE abap_bool,
      mv_static     TYPE abap_bool.

    METHODS: parse_uml_code REDEFINITION.
  PRIVATE SECTION.
ENDCLASS.



CLASS ZDAB_CL_UML_CLASS_METHOD_API IMPLEMENTATION.


  METHOD get_abstract_indicator.
    rv_abstract = mv_abstract.
  ENDMETHOD.


  METHOD set_visibility.
    mv_visibility = iv_visibility.
  ENDMETHOD.


  METHOD set_static_indicator.
    me->mv_static = iv_static.
  ENDMETHOD.


  METHOD get_static_indicator.
    rv_static = mv_static.
  ENDMETHOD.


  METHOD get_visibility.
    rv_visibility = mv_visibility.
  ENDMETHOD.


  METHOD parse_uml_code.
    " +{static} {abstract} FieldName : FieldType
    rv_uml_code = |{ mv_visibility }| &&
                  |{ COND #( WHEN mv_static = abap_true THEN |{ gc_curly_brace_open }static{ gc_curly_brace_closed } | ) }| &&
                  |{ COND #( WHEN mv_abstract = abap_true THEN |{ gc_curly_brace_open }abstract{ gc_curly_brace_closed } | ) }| &&
                  |{ to_lower( mv_element_name ) }()| &&
                  |{ gc_new_line }|   ##NO_TEXT.

  ENDMETHOD.


  METHOD set_abstract_indicator.
    me->mv_abstract = iv_abstract.
  ENDMETHOD.
ENDCLASS.
