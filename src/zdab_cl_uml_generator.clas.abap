CLASS zdab_cl_uml_generator DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    TYPES:
      ty_ddic_object_type TYPE c LENGTH 4,
      ty_ddic_object_name TYPE c LENGTH 40,

      BEGIN OF ty_s_ddic_object,
        object_type TYPE ty_ddic_object_type,
        object_name TYPE ty_ddic_object_name,
      END OF ty_s_ddic_object,

      BEGIN OF ty_s_diagram_config,
        types              TYPE abap_boolean,
        constants          TYPE abap_boolean,
        alias              TYPE abap_boolean,
        attributes         TYPE abap_boolean,
        methods            TYPE abap_boolean,
        relationships      TYPE abap_boolean,
        deep_relationships TYPE abap_boolean,
      END OF ty_s_diagram_config,

      ty_t_ddic_object TYPE STANDARD TABLE OF ty_s_ddic_object.

    CONSTANTS:
      BEGIN OF gc_ddic_object_type,
        class     TYPE ty_ddic_object_type VALUE 'CLAS',
        interface TYPE ty_ddic_object_type VALUE 'INTF',
      END OF gc_ddic_object_type.

    METHODS:
      constructor
        IMPORTING is_diagram_config TYPE ty_s_diagram_config,
      generate_uml_element
        IMPORTING is_object             TYPE ty_s_ddic_object
        RETURNING VALUE(ro_uml_element) TYPE REF TO zdab_if_uml_element_api,
      generate_uml_elements
        IMPORTING it_objects            TYPE ty_t_ddic_object
        RETURNING VALUE(rt_uml_element) TYPE zdab_t_uml_element,
      generate_uml_element_relations
        IMPORTING it_objects            TYPE ty_t_ddic_object
        RETURNING VALUE(rt_uml_element) TYPE zdab_t_uml_element,
      generate_uml_diagram
        IMPORTING iv_diagram_name       TYPE string
                  it_objects            TYPE ty_t_ddic_object
        RETURNING VALUE(ro_uml_diagram) TYPE REF TO zdab_if_uml_element_api,
      generate_uml_diagrm_relations
        IMPORTING iv_diagram_name       TYPE string
                  it_objects            TYPE ty_t_ddic_object
        RETURNING VALUE(ro_uml_diagram) TYPE REF TO zdab_if_uml_element_api.

  PROTECTED SECTION.
  PRIVATE SECTION.
    DATA:
      ms_diagram_config TYPE ty_s_diagram_config.
ENDCLASS.

CLASS zdab_cl_uml_generator IMPLEMENTATION.

  METHOD constructor.
    ms_diagram_config = CORRESPONDING #( is_diagram_config ).
  ENDMETHOD.

  METHOD generate_uml_elements.

    LOOP AT it_objects ASSIGNING FIELD-SYMBOL(<ls_objects>).
      CASE <ls_objects>-object_type.
        WHEN gc_ddic_object_type-class.

          DATA(lo_class_parser) = NEW zdab_cl_uml_parser4class( CONV #( <ls_objects>-object_name ) )->add_definition( ).

          IF ms_diagram_config-alias = abap_true.
            lo_class_parser->add_alias( ).
          ENDIF.

          IF ms_diagram_config-attributes = abap_true.
            lo_class_parser->add_attributes( ).
          ENDIF.

          IF ms_diagram_config-constants = abap_true.
            lo_class_parser->add_constants( ).
          ENDIF.

          IF ms_diagram_config-types = abap_true.
            lo_class_parser->add_types( ).
          ENDIF.

          IF ms_diagram_config-methods = abap_true.
            lo_class_parser->add_methods( ).
          ENDIF.

          IF ms_diagram_config-relationships = abap_true.
            lo_class_parser->add_relationships(
              EXPORTING
                iv_types              = ms_diagram_config-types
                iv_constants          = ms_diagram_config-constants
                iv_alias              = ms_diagram_config-alias
                iv_attributes         = ms_diagram_config-attributes
                iv_methods            = ms_diagram_config-methods
                iv_relationships      = ms_diagram_config-relationships
                iv_deep_relationships = ms_diagram_config-deep_relationships
            ).
          ENDIF.

          IF ms_diagram_config-deep_relationships = abap_true.
            lo_class_parser->add_deep_relationships(
              EXPORTING
                iv_types              = ms_diagram_config-types
                iv_constants          = ms_diagram_config-constants
                iv_alias              = ms_diagram_config-alias
                iv_attributes         = ms_diagram_config-attributes
                iv_methods            = ms_diagram_config-methods
                iv_relationships      = ms_diagram_config-relationships
                iv_deep_relationships = ms_diagram_config-deep_relationships
            ).
          ENDIF.

          APPEND lo_class_parser->get_uml_element( ) TO rt_uml_element.
      ENDCASE.

    ENDLOOP.

  ENDMETHOD.

  METHOD generate_uml_diagrm_relations.
    ro_uml_diagram = NEW zdab_cl_uml_diagram_api( iv_diagram_name ).

    DATA(lt_elements) = generate_uml_element_relations( it_objects ).

    LOOP AT lt_elements INTO DATA(lo_elements).
      ro_uml_diagram->add_child_element( lo_elements ).
    ENDLOOP.
  ENDMETHOD.


  METHOD generate_uml_element_relations.

  ENDMETHOD.

  METHOD generate_uml_diagram.
    ro_uml_diagram = NEW zdab_cl_uml_diagram_api( iv_diagram_name ).

    LOOP AT generate_uml_elements( it_objects ) INTO DATA(lo_elements).
      ro_uml_diagram->add_child_element( lo_elements ).
    ENDLOOP.
  ENDMETHOD.

  METHOD generate_uml_element.
    DATA(lt_elements) = generate_uml_elements( it_objects = VALUE #( ( object_type = is_object-object_type
                                                                       object_name = is_object-object_name ) ) ).

    ro_uml_element = lt_elements[ 1 ].
  ENDMETHOD.

ENDCLASS.
