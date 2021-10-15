CLASS zdab_cl_uml_element_relate_api DEFINITION
  PUBLIC
  CREATE PRIVATE .

  PUBLIC SECTION.

    TYPES:
      BEGIN OF ty_s_adding_relation,
        source_name   TYPE sxco_ao_object_name,
        target_name   TYPE sxco_ao_object_name,
        target_type   TYPE string,
        relation_type TYPE string,
      END OF ty_s_adding_relation,

      BEGIN OF ty_s_related_element,
        target_name    TYPE sxco_ao_object_name,
        target_type    TYPE string,
        target_element TYPE REF TO zdab_if_uml_element_api,
      END OF ty_s_related_element,

      ty_t_adding_relation TYPE STANDARD TABLE OF ty_s_adding_relation,
      ty_t_related_element TYPE SORTED TABLE OF ty_s_related_element WITH UNIQUE KEY primary_key COMPONENTS target_name target_type,
      ty_t_relation        TYPE SORTED TABLE OF ty_s_adding_relation WITH UNIQUE KEY primary_key COMPONENTS source_name target_name target_type relation_type.

    CLASS-METHODS:
      get_instance
        RETURNING VALUE(ro_instance) TYPE REF TO zdab_cl_uml_element_relate_api.

    METHODS:
      add_relation
        IMPORTING is_adding_relation TYPE ty_s_adding_relation,

      add_relations
        IMPORTING it_adding_relation TYPE ty_t_adding_relation,

      set_related_element
        IMPORTING iv_target_name    TYPE sxco_ao_object_name
                  iv_target_type    TYPE string
                  io_target_element TYPE REF TO zdab_if_uml_element_api,

      get_relations
        EXPORTING
          et_relations TYPE ty_t_adding_relation,

      get_related_elements
        EXPORTING
          et_related_elements TYPE ty_t_related_element,

      get_relations_uml_code
        RETURNING VALUE(rv_relations_uml_code) TYPE string.

  PROTECTED SECTION.
  PRIVATE SECTION.
    CLASS-DATA:
      mo_instance TYPE REF TO zdab_cl_uml_element_relate_api.

    DATA:
      mt_adding_relations TYPE ty_t_adding_relation,
      mt_related_elements TYPE ty_t_related_element,
      mt_relations        TYPE ty_t_relation.
ENDCLASS.

CLASS zdab_cl_uml_element_relate_api IMPLEMENTATION.

  METHOD get_instance.
    IF mo_instance IS NOT BOUND.
      mo_instance = NEW #( ).
    ENDIF.

    ro_instance = mo_instance.
  ENDMETHOD.

  METHOD add_relation.

    CHECK is_adding_relation-source_name <> is_adding_relation-target_name.

    APPEND is_adding_relation TO mt_adding_relations.

    IF NOT line_exists( mt_related_elements[ KEY primary_key COMPONENTS target_name = is_adding_relation-target_name
                                                                        target_type = is_adding_relation-target_type ] ).

      INSERT CORRESPONDING #( is_adding_relation ) INTO TABLE mt_related_elements.

    ENDIF.

    IF NOT line_exists( mt_relations[ KEY primary_key COMPONENTS source_name   = is_adding_relation-source_name
                                                                 target_name   = is_adding_relation-target_name
                                                                 target_type   = is_adding_relation-target_type
                                                                 relation_type = is_adding_relation-relation_type ] ).

      INSERT is_adding_relation INTO TABLE mt_relations.

    ENDIF.

  ENDMETHOD.


  METHOD add_relations.

    LOOP AT it_adding_relation ASSIGNING FIELD-SYMBOL(<ls_adding_relation>).
      add_relation( <ls_adding_relation> ).
    ENDLOOP.

  ENDMETHOD.


  METHOD set_related_element.
    READ TABLE mt_related_elements ASSIGNING FIELD-SYMBOL(<ls_related_elements>)
      WITH KEY primary_key COMPONENTS target_name = iv_target_name
                                      target_type = iv_target_type.

    IF sy-subrc = 0.
      <ls_related_elements>-target_element = io_target_element.
    ENDIF.
  ENDMETHOD.


  METHOD get_relations.
    et_relations = mt_relations.
  ENDMETHOD.


  METHOD get_related_elements.
    et_related_elements = mt_related_elements.
  ENDMETHOD.


  METHOD get_relations_uml_code.

    DATA:
      lv_relations        TYPE string,
      lv_related_elements TYPE string.

    LOOP AT mt_related_elements ASSIGNING FIELD-SYMBOL(<ls_related_elements>)
      WHERE target_element IS BOUND.

      lv_related_elements = |{ lv_related_elements }| &&
                            |{ <ls_related_elements>-target_element->get_uml_code( ) }| &&
                            |{ zdab_if_uml_element_api=>gc_new_line }|.

    ENDLOOP.

    LOOP AT mt_relations ASSIGNING FIELD-SYMBOL(<ls_relation>).

      lv_relations = |{ lv_relations }| &&
                     |{ to_lower( <ls_relation>-target_name ) } | &&
                     |{ <ls_relation>-relation_type } | &&
                     |{ to_lower( <ls_relation>-source_name ) }| &&
                     |{ zdab_if_uml_element_api=>gc_new_line }|.

    ENDLOOP.

    rv_relations_uml_code = |{ lv_related_elements } { lv_relations }|.

*    LOOP AT mt_relations ASSIGNING FIELD-SYMBOL(<ls_relation>).
*
*      DATA(lo_related_element) = <ls_relation>-target_element.
*      DATA(lv_target_class_name) = cl_abap_classdescr=>get_class_name( lo_related_element ).
*      DATA(lv_related_element_name) = lo_related_element->get_name_element( ).
*
*      DATA(lo_source_element) = <ls_relation>-source_element.
*      DATA(lv_source_class_name) = cl_abap_classdescr=>get_class_name( lo_source_element ).
*      DATA(lv_source_element_name) = lo_source_element->get_name_element( ).
*
*      IF NOT line_exists( mt_added_relations[ KEY primary_key COMPONENTS source_name   = lv_source_element_name
*                                                                         target_name   = lv_related_element_name
*                                                                         relation_type = <ls_relation>-relation_type ] ).
*
*        lv_relations = |{ lv_relations }| &&
*                       |{ to_lower( <ls_relation>-target_element->get_name_element( ) ) } | &&
*                       |{ <ls_relation>-relation_type } | &&
*                       |{ to_lower( <ls_relation>-source_element->get_name_element( ) ) }| &&
*                       |{ zdab_if_uml_element_api=>gc_new_line }|.
*
*        APPEND VALUE #( source_name   = lv_source_element_name
*                        target_name   = lv_related_element_name
*                        relation_type = <ls_relation>-relation_type ) TO mt_added_relations.
*
*      ENDIF.
*
*      IF NOT line_exists( mt_added_related_elements[ KEY primary_key COMPONENTS class_name = lv_target_class_name
*                                                                                element_name = lv_related_element_name ] ).
*        lv_related_elements = |{ lv_related_elements }| &&
*                              |{ lo_related_element->get_uml_code( ) }| &&
*                              |{ zdab_if_uml_element_api=>gc_new_line }|.
*
*        APPEND VALUE #( class_name = lv_target_class_name
*                        element_name = lv_related_element_name ) TO mt_added_related_elements.
*
*      ENDIF.
*
*    ENDLOOP.
*
*    rv_relations_uml_code = |{ lv_related_elements } { lv_relations }|.

  ENDMETHOD.

ENDCLASS.
