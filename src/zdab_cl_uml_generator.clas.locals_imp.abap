CLASS lcl_abap_class2uml_parser DEFINITION CREATE PUBLIC.
  		
  PUBLIC SECTION.
    METHODS:
      constructor
        IMPORTING iv_name TYPE sxco_ao_object_name,

      parse_abap2uml_element
        IMPORTING iv_get_details        TYPE abap_bool DEFAULT abap_false
                  iv_get_relations      TYPE abap_bool DEFAULT abap_false
                  iv_get_deep_relations TYPE abap_bool DEFAULT abap_false
        RETURNING VALUE(ro_uml_element) TYPE REF TO zdab_if_uml_element_api.

  PROTECTED SECTION.
  PRIVATE SECTION.
    DATA:
      mv_class_name TYPE sxco_ao_object_name,
      mo_xco_class  TYPE REF TO if_xco_ao_class.

ENDCLASS.

CLASS lcl_abap_interface2uml_parser DEFINITION CREATE PUBLIC.

  PUBLIC SECTION.
    METHODS:
      constructor
        IMPORTING iv_name TYPE sxco_ao_object_name,

      parse_abap2uml_element
        IMPORTING iv_get_details        TYPE abap_bool DEFAULT abap_false
                  iv_get_relations      TYPE abap_bool DEFAULT abap_false
                  iv_get_deep_relations TYPE abap_bool DEFAULT abap_false
        RETURNING VALUE(ro_uml_element) TYPE REF TO zdab_if_uml_element_api.

  PROTECTED SECTION.
  PRIVATE SECTION.
    DATA:
      mv_interface_name TYPE sxco_ao_object_name,
      mo_xco_interface  TYPE REF TO if_xco_ao_interface.

ENDCLASS.

CLASS lcl_abap_class2uml_parser IMPLEMENTATION.

  METHOD constructor.
    mv_class_name = iv_name.
    mo_xco_class = xco_cp_abap=>class( mv_class_name ).
  ENDMETHOD.

  METHOD parse_abap2uml_element.

    DATA(lo_uml_class) = NEW zdab_cl_uml_class_api( |{ mv_class_name }| ).

******************************** CLASS DEFINITION ********************************
    DATA(ls_definition_content) = mo_xco_class->definition->content( )->get( ).

    " Set UML Class attributes
    IF ls_definition_content-abstract_indicator = abap_true.
      lo_uml_class->set_class_type( iv_class_type = zdab_if_uml_element_api=>gc_class_type-abstract ).
    ELSE.
      lo_uml_class->set_class_type( iv_class_type = zdab_if_uml_element_api=>gc_class_type-class ).
    ENDIF.

    IF iv_get_details = abap_true.

******************************** CLASS TYPES *************************************

      " PUBLIC TYPES
      LOOP AT mo_xco_class->definition->section-public->components->type->all->get( ) INTO DATA(lo_public_types).

        DATA(lo_uml_public_types) = NEW zdab_cl_uml_class_types_api( |{ lo_public_types->name }| ).
        lo_uml_public_types->set_visibility( zdab_if_uml_element_api=>gc_visibility-public ).
        lo_uml_public_types->set_field_type( lo_public_types->content( )->get_typing_definition( )->get_value( ) ).

        lo_uml_class->add_child_element( lo_uml_public_types ).

      ENDLOOP.

      " PRIVATE TYPES
      LOOP AT mo_xco_class->definition->section-private->components->type->all->get( ) INTO DATA(lo_private_types).

        DATA(lo_uml_private_types) = NEW zdab_cl_uml_class_types_api( |{ lo_private_types->name }| ).
        lo_uml_private_types->set_visibility( zdab_if_uml_element_api=>gc_visibility-public ).
        lo_uml_private_types->set_field_type( lo_private_types->content( )->get_typing_definition( )->get_value( ) ).

        lo_uml_class->add_child_element( lo_uml_private_types ).

      ENDLOOP.

      " PROTECTED TYPES
      LOOP AT mo_xco_class->definition->section-protected->components->type->all->get( ) INTO DATA(lo_protected_types).

        DATA(lo_uml_protected_types) = NEW zdab_cl_uml_class_types_api( |{ lo_protected_types->name }| ).
        lo_uml_protected_types->set_visibility( zdab_if_uml_element_api=>gc_visibility-protected ).
        lo_uml_protected_types->set_field_type( lo_protected_types->content( )->get_typing_definition( )->get_value( ) ).

        lo_uml_class->add_child_element( lo_uml_protected_types ).

      ENDLOOP.

******************************** CLASS CONSTANTS **********************************

      " PUBLIC CONSTANTS
      LOOP AT mo_xco_class->definition->section-public->components->constant->all->get( ) INTO DATA(lo_public_constants).

        DATA(lo_uml_public_constants) = NEW zdab_cl_uml_class_constant_api( |{ lo_public_constants->name }| ).
        lo_uml_public_constants->set_visibility( zdab_if_uml_element_api=>gc_visibility-public ).

        DATA(lo_public_constant_content) = lo_public_constants->content( ).
        IF lo_public_constant_content IS BOUND.
          lo_uml_public_constants->set_field_type( lo_public_constant_content->get_typing_definition( )->get_value( ) ).
          lo_uml_public_constants->set_value( lo_public_constant_content->get_value( ) ).
        ENDIF.

        lo_uml_class->add_child_element( lo_uml_public_constants ).

      ENDLOOP.

      " PRIVATE CONSTANTS
      LOOP AT mo_xco_class->definition->section-private->components->constant->all->get( ) INTO DATA(lo_private_constants).

        DATA(lo_uml_private_constants) = NEW zdab_cl_uml_class_constant_api( |{ lo_private_constants->name }| ).
        lo_uml_private_constants->set_visibility( zdab_if_uml_element_api=>gc_visibility-public ).

        DATA(lo_private_constant_content) = lo_private_constants->content( ).
        IF lo_private_constant_content IS BOUND.
          lo_uml_private_constants->set_field_type( lo_private_constant_content->get_typing_definition( )->get_value( ) ).
          lo_uml_private_constants->set_value( lo_private_constant_content->get_value( ) ).
        ENDIF.

        lo_uml_class->add_child_element( lo_uml_private_constants ).

      ENDLOOP.

      " PROTECTED CONSTANTS
      LOOP AT mo_xco_class->definition->section-protected->components->constant->all->get( ) INTO DATA(lo_protected_constants).

        DATA(lo_uml_protected_constants) = NEW zdab_cl_uml_class_constant_api( |{ lo_protected_constants->name }| ).
        lo_uml_protected_constants->set_visibility( zdab_if_uml_element_api=>gc_visibility-protected ).

        DATA(lo_protected_constant_content) = lo_protected_constants->content( ).
        IF lo_protected_constant_content IS BOUND.
          lo_uml_protected_constants->set_field_type( lo_protected_constant_content->get_typing_definition( )->get_value( ) ).
          lo_uml_protected_constants->set_value( lo_protected_constant_content->get_value( ) ).
        ENDIF.

        lo_uml_class->add_child_element( lo_uml_protected_constants ).

      ENDLOOP.

******************************** CLASS ALIAS **********************************

      " PUBLIC ALIAS
      LOOP AT mo_xco_class->definition->section-public->components->alias->all->get( ) INTO DATA(lo_public_alias).

        TRY.
            DATA(lo_uml_public_alias) = NEW zdab_cl_uml_class_alias_api( |{ lo_public_alias->name }| ).
            lo_uml_public_alias->set_visibility( zdab_if_uml_element_api=>gc_visibility-public ).
            lo_uml_public_alias->set_interface( lo_public_alias->content( )->get_interface( )->name ).
            lo_uml_public_alias->set_component( lo_public_alias->content( )->get_component( ) ).

            lo_uml_class->add_child_element( lo_uml_public_alias ).
          CATCH cx_root.
            CONTINUE.
        ENDTRY.

      ENDLOOP.

      " PRIVATE ALIAS
      LOOP AT mo_xco_class->definition->section-private->components->alias->all->get( ) INTO DATA(lo_private_alias).
        TRY.
            DATA(lo_uml_private_alias) = NEW zdab_cl_uml_class_alias_api( |{ lo_private_alias->name }| ).
            lo_uml_private_alias->set_visibility( zdab_if_uml_element_api=>gc_visibility-public ).
            lo_uml_private_alias->set_interface( lo_private_alias->content( )->get_interface( )->name ).
            lo_uml_private_alias->set_component( lo_private_alias->content( )->get_component( ) ).

            lo_uml_class->add_child_element( lo_uml_private_alias ).
          CATCH cx_root.
            CONTINUE.
        ENDTRY.

      ENDLOOP.

      " PROTECTED ALIAS
      LOOP AT mo_xco_class->definition->section-protected->components->alias->all->get( ) INTO DATA(lo_protected_alias).
        TRY.
            DATA(lo_uml_protected_alias) = NEW zdab_cl_uml_class_alias_api( |{ lo_protected_alias->name }| ).
            lo_uml_protected_alias->set_visibility( zdab_if_uml_element_api=>gc_visibility-protected ).
            lo_uml_protected_alias->set_interface( lo_protected_alias->content( )->get_interface( )->name ).
            lo_uml_protected_alias->set_component( lo_protected_alias->content( )->get_component( ) ).

            lo_uml_class->add_child_element( lo_uml_protected_alias ).
          CATCH cx_root.
            CONTINUE.
        ENDTRY.

      ENDLOOP.

******************************** CLASS DATA **************************************

      " PUBLIC CLASS DATA
      LOOP AT mo_xco_class->definition->section-public->components->class_data->all->get( ) INTO DATA(lo_public_class_data).

        DATA(lo_uml_public_class_data) = NEW zdab_cl_uml_class_field_api( |{ lo_public_class_data->name }| ).
        lo_uml_public_class_data->set_visibility( zdab_if_uml_element_api=>gc_visibility-public ).
        lo_uml_public_class_data->set_static_indicator( abap_true ).
        lo_uml_public_class_data->set_field_type( lo_public_class_data->content( )->get_typing_definition( )->get_value( ) ).

        lo_uml_class->add_child_element( lo_uml_public_class_data ).

      ENDLOOP.

      " PRIVATE CLASS DATA
      LOOP AT mo_xco_class->definition->section-private->components->class_data->all->get( ) INTO DATA(lo_private_class_data).

        DATA(lo_uml_private_class_data) = NEW zdab_cl_uml_class_field_api( |{ lo_private_class_data->name }| ).
        lo_uml_private_class_data->set_visibility( zdab_if_uml_element_api=>gc_visibility-private ).
        lo_uml_private_class_data->set_static_indicator( abap_true ).
        lo_uml_private_class_data->set_field_type( lo_private_class_data->content( )->get_typing_definition( )->get_value( ) ).

        lo_uml_class->add_child_element( lo_uml_private_class_data ).

      ENDLOOP.

      " PROTECTED CLASS DATA
      LOOP AT mo_xco_class->definition->section-protected->components->class_data->all->get( ) INTO DATA(lo_protected_class_data).

        DATA(lo_uml_protected_class_data) = NEW zdab_cl_uml_class_field_api( |{ lo_protected_class_data->name }| ).
        lo_uml_protected_class_data->set_visibility( zdab_if_uml_element_api=>gc_visibility-protected ).
        lo_uml_protected_class_data->set_static_indicator( abap_true ).
        lo_uml_protected_class_data->set_field_type( lo_protected_class_data->content( )->get_typing_definition( )->get_value( ) ).

        lo_uml_class->add_child_element( lo_uml_protected_class_data ).

      ENDLOOP.

      " PUBLIC DATA
      LOOP AT mo_xco_class->definition->section-public->components->data->all->get( ) INTO DATA(lo_public_data).

        DATA(lo_uml_public_data) = NEW zdab_cl_uml_class_field_api( |{ lo_public_data->name }| ).
        lo_uml_public_data->set_visibility( zdab_if_uml_element_api=>gc_visibility-public ).
        lo_uml_public_data->set_field_type( lo_public_data->content( )->get_typing_definition( )->get_value( ) ).

        lo_uml_class->add_child_element( lo_uml_public_data ).

      ENDLOOP.

      " PRIVATE DATA
      LOOP AT mo_xco_class->definition->section-private->components->data->all->get( ) INTO DATA(lo_private_data).

        DATA(lo_uml_private_data) = NEW zdab_cl_uml_class_field_api( |{ lo_private_data->name }| ).
        lo_uml_private_data->set_visibility( zdab_if_uml_element_api=>gc_visibility-private ).
        lo_uml_private_data->set_field_type( lo_private_data->content( )->get_typing_definition( )->get_value( ) ).

        lo_uml_class->add_child_element( lo_uml_private_data ).

      ENDLOOP.

      " PROTECTED DATA
      LOOP AT mo_xco_class->definition->section-protected->components->data->all->get( ) INTO DATA(lo_protected_data).

        DATA(lo_uml_protected_data) = NEW zdab_cl_uml_class_field_api( |{ lo_protected_data->name }| ).
        lo_uml_protected_data->set_visibility( zdab_if_uml_element_api=>gc_visibility-protected ).
        lo_uml_protected_data->set_field_type( lo_protected_data->content( )->get_typing_definition( )->get_value( ) ).

        lo_uml_class->add_child_element( lo_uml_protected_data ).

      ENDLOOP.

******************************** CLASS METHODS **************************************

      " PUBLIC CLASS METHOD
      LOOP AT mo_xco_class->definition->section-public->components->class_method->all->get( ) INTO DATA(lo_public_class_methods).

        DATA(lo_uml_public_class_method) = NEW zdab_cl_uml_class_method_api( |{ lo_public_class_methods->name }| ).
        lo_uml_public_class_method->set_visibility( zdab_if_uml_element_api=>gc_visibility-public ).
        lo_uml_public_class_method->set_static_indicator( abap_true ).

        lo_uml_class->add_child_element( lo_uml_public_class_method ).

      ENDLOOP.

      " PRIVATE CLASS METHOD
      LOOP AT mo_xco_class->definition->section-private->components->class_method->all->get( ) INTO DATA(lo_private_class_methods).

        DATA(lo_uml_private_class_method) = NEW zdab_cl_uml_class_method_api( |{ lo_private_class_methods->name }| ).
        lo_uml_private_class_method->set_visibility( zdab_if_uml_element_api=>gc_visibility-private ).
        lo_uml_private_class_method->set_static_indicator( abap_true ).

        lo_uml_class->add_child_element( lo_uml_private_class_method ).

      ENDLOOP.

      " PROTECTED CLASS METHOD
      LOOP AT mo_xco_class->definition->section-protected->components->class_method->all->get( ) INTO DATA(lo_protected_class_methods).

        DATA(lo_uml_protected_class_method) = NEW zdab_cl_uml_class_method_api( |{ lo_protected_class_methods->name }| ).
        lo_uml_protected_class_method->set_visibility( zdab_if_uml_element_api=>gc_visibility-protected ).
        lo_uml_protected_class_method->set_static_indicator( abap_true ).

        lo_uml_class->add_child_element( lo_uml_protected_class_method ).

      ENDLOOP.

      " PUBLIC METHODS
      LOOP AT mo_xco_class->definition->section-public->components->method->all->get( ) INTO DATA(lo_public_method).

        DATA(lo_uml_public_method) = NEW zdab_cl_uml_class_method_api( |{ lo_public_method->name }| ).
        lo_uml_public_method->set_visibility( zdab_if_uml_element_api=>gc_visibility-public ).

        lo_uml_class->add_child_element( lo_uml_public_method ).

      ENDLOOP.

      " PRIVATE METHODS
      LOOP AT mo_xco_class->definition->section-private->components->method->all->get( ) INTO DATA(lo_private_method).

        DATA(lo_uml_private_method) = NEW zdab_cl_uml_class_method_api( |{ lo_private_method->name }| ).
        lo_uml_private_method->set_visibility( zdab_if_uml_element_api=>gc_visibility-private ).

        lo_uml_class->add_child_element( lo_uml_private_method ).

      ENDLOOP.

      " PROTECTED METHODS
      LOOP AT mo_xco_class->definition->section-protected->components->method->all->get( ) INTO DATA(lo_protected_method).

        DATA(lo_uml_protected_method) = NEW zdab_cl_uml_class_method_api( |{ lo_protected_method->name }| ).
        lo_uml_protected_method->set_visibility( zdab_if_uml_element_api=>gc_visibility-protected ).

        lo_uml_class->add_child_element( lo_uml_protected_method ).

      ENDLOOP.

    ENDIF.

******************************** CLASS RELATIONSHIPS ****************************

    IF iv_get_relations = abap_true.
      " SUPER CLASS
      IF ls_definition_content-superclass IS NOT INITIAL.

        lo_uml_class->set_super_class( NEW lcl_abap_class2uml_parser( ls_definition_content-superclass->name )->parse_abap2uml_element(
                                                                                                                  iv_get_details   = abap_true
                                                                                                                  iv_get_relations = abap_true
                                                                                                                ) ).
      ENDIF.

      " INTERFACES
      LOOP AT ls_definition_content-interfaces INTO DATA(lo_interface).
        lo_uml_class->set_interface( NEW lcl_abap_interface2uml_parser( lo_interface->name )->parse_abap2uml_element(
                                                                                                iv_get_details   = abap_true
                                                                                                iv_get_relations = abap_true
                                                                                              ) ).
      ENDLOOP.

    ENDIF.

    ro_uml_element = lo_uml_class.

  ENDMETHOD.

ENDCLASS.

CLASS lcl_abap_interface2uml_parser IMPLEMENTATION.

  METHOD constructor.
    mv_interface_name = iv_name.
    mo_xco_interface = xco_cp_abap=>interface( mv_interface_name ).
  ENDMETHOD.

  METHOD parse_abap2uml_element.

    DATA(lo_uml_interface) = NEW zdab_cl_uml_class_api( |{ mv_interface_name }| ).
    lo_uml_interface->set_class_type( zdab_if_uml_element_api=>gc_class_type-interface ).

    IF iv_get_details = abap_true.

******************************** INTERFACE TYPES *************************************

      " PUBLIC TYPES
      LOOP AT mo_xco_interface->components->type->all->get( ) INTO DATA(lo_public_types).

        DATA(lo_uml_public_types) = NEW zdab_cl_uml_class_types_api( |{ lo_public_types->name }| ).
        lo_uml_public_types->set_visibility( zdab_if_uml_element_api=>gc_visibility-public ).
        lo_uml_public_types->set_field_type( lo_public_types->content( )->get_typing_definition( )->get_value( ) ).

        lo_uml_interface->add_child_element( lo_uml_public_types ).

      ENDLOOP.

******************************** INTERFACE CONSTANTS **********************************

      " PUBLIC CONSTANTS
      LOOP AT mo_xco_interface->components->constant->all->get( ) INTO DATA(lo_public_constants).

        DATA(lo_uml_public_constants) = NEW zdab_cl_uml_class_constant_api( |{ lo_public_constants->name }| ).
        lo_uml_public_constants->set_visibility( zdab_if_uml_element_api=>gc_visibility-public ).

        DATA(lo_public_constant_content) = lo_public_constants->content( ).
        IF lo_public_constant_content IS BOUND.
          lo_uml_public_constants->set_field_type( lo_public_constant_content->get_typing_definition( )->get_value( ) ).
          lo_uml_public_constants->set_value( lo_public_constant_content->get_value( ) ).
        ENDIF.

        lo_uml_interface->add_child_element( lo_uml_public_constants ).

      ENDLOOP.

******************************** INTERFACE ALIAS **********************************

      " PUBLIC ALIAS
      LOOP AT mo_xco_interface->components->alias->all->get( ) INTO DATA(lo_public_alias).

        DATA(lo_uml_public_alias) = NEW zdab_cl_uml_class_alias_api( |{ lo_public_alias->name }| ).
        lo_uml_public_alias->set_visibility( zdab_if_uml_element_api=>gc_visibility-public ).
        lo_uml_public_alias->set_interface( lo_public_alias->content( )->get_interface( )->name ).
        lo_uml_public_alias->set_component( lo_public_alias->content( )->get_component( ) ).

        lo_uml_interface->add_child_element( lo_uml_public_alias ).

      ENDLOOP.

******************************** INTERFACE DATA **************************************

      " PUBLIC INTERFACE DATA
      LOOP AT mo_xco_interface->components->class_data->all->get( ) INTO DATA(lo_public_class_data).

        DATA(lo_uml_public_interface_data) = NEW zdab_cl_uml_class_field_api( |{ lo_public_class_data->name }| ).
        lo_uml_public_interface_data->set_visibility( zdab_if_uml_element_api=>gc_visibility-public ).
        lo_uml_public_interface_data->set_static_indicator( abap_true ).
        lo_uml_public_interface_data->set_field_type( lo_public_class_data->content( )->get_typing_definition( )->get_value( ) ).

        lo_uml_interface->add_child_element( lo_uml_public_interface_data ).

      ENDLOOP.

      " PUBLIC DATA
      LOOP AT mo_xco_interface->components->data->all->get( ) INTO DATA(lo_public_data).

        DATA(lo_uml_public_data) = NEW zdab_cl_uml_class_field_api( |{ lo_public_data->name }| ).
        lo_uml_public_data->set_visibility( zdab_if_uml_element_api=>gc_visibility-public ).
        lo_uml_public_data->set_field_type( lo_public_data->content( )->get_typing_definition( )->get_value( ) ).

        lo_uml_interface->add_child_element( lo_uml_public_data ).

      ENDLOOP.

******************************** INTERFACE METHODS **************************************

      " PUBLIC INTERFACE METHOD
      LOOP AT mo_xco_interface->components->class_method->all->get( ) INTO DATA(lo_public_interface_methods).

        DATA(lo_uml_public_interface_method) = NEW zdab_cl_uml_class_method_api( |{ lo_public_interface_methods->name }| ).
        lo_uml_public_interface_method->set_visibility( zdab_if_uml_element_api=>gc_visibility-public ).
        lo_uml_public_interface_method->set_static_indicator( abap_true ).

        lo_uml_interface->add_child_element( lo_uml_public_interface_method ).

      ENDLOOP.

      " PUBLIC METHODS
      LOOP AT mo_xco_interface->components->method->all->get( ) INTO DATA(lo_public_method).

        DATA(lo_uml_public_method) = NEW zdab_cl_uml_class_method_api( |{ lo_public_method->name }| ).
        lo_uml_public_method->set_visibility( zdab_if_uml_element_api=>gc_visibility-public ).

        lo_uml_interface->add_child_element( lo_uml_public_method ).

      ENDLOOP.

    ENDIF.

******************************** INTERFACE RELATIONSHIPS ****************************

    IF iv_get_relations = abap_true.

      " INTERFACES
      LOOP AT mo_xco_interface->content( )->get_interfaces( ) INTO DATA(lo_interface).
        lo_uml_interface->set_interface( NEW lcl_abap_interface2uml_parser( lo_interface->name )->parse_abap2uml_element(
                                                                                                    iv_get_details   = abap_true
                                                                                                    iv_get_relations = abap_true
                                                                                                  ) ).
      ENDLOOP.

    ENDIF.

    ro_uml_element = lo_uml_interface.

  ENDMETHOD.

ENDCLASS.
