INTERFACE zdab_if_uml_element_api
  PUBLIC.

  CONSTANTS:
    gc_new_line           TYPE abap_char1 VALUE cl_abap_char_utilities=>newline,
    gc_curly_brace_open   TYPE string VALUE '{'   ##NO_TEXT,
    gc_curly_brace_closed TYPE string VALUE '}'   ##NO_TEXT,

    BEGIN OF gc_class_type,
      interface TYPE string VALUE 'interface' ##NO_TEXT,
      class     TYPE string VALUE 'class'     ##NO_TEXT,
      abstract  TYPE string VALUE 'abstract'  ##NO_TEXT,
    END OF   gc_class_type,

    BEGIN OF gc_visibility,
      private   TYPE string VALUE '-'     ##NO_TEXT,
      protected TYPE string VALUE '#'     ##NO_TEXT,
      public    TYPE string VALUE '+'     ##NO_TEXT,
    END OF   gc_visibility,

    BEGIN OF gc_relation_type,
      implementation TYPE string VALUE '<|..'    ##NO_TEXT,
      extension      TYPE string VALUE '<|--'    ##NO_TEXT,
      composition    TYPE string VALUE '*--'     ##NO_TEXT,
      aggregation    TYPE string VALUE 'o--'     ##NO_TEXT,
    END OF   gc_relation_type.

*Для двух объектов Foo и Bar отношения могут быть определены
*
*Ассоциация - у меня есть связь с объектом. Foo использует Bar
*
*public class Foo {
*    void Baz(Bar bar) {
*    }
*};
*Композиция - я владею объектом и несу ответственность за его жизнь. Когда умирает Foo , умирает и Bar
*
*public class Foo {
*    private Bar bar = new Bar();
*}
*Агрегация - У меня есть объект, который я позаимствовал у кого-то другого. Когда Foo умрет, Bar может жить дальше.
*
*public class Foo {
*    private Bar bar;
*    Foo(Bar bar) {
*       this.bar = bar;
*    }
*}

  METHODS:
    set_name_element
      IMPORTING iv_element_name TYPE string,
    get_name_element
      RETURNING VALUE(rv_element_name) TYPE string,

    set_parent_element
      IMPORTING io_parent_element TYPE REF TO zdab_if_uml_element_api,
    get_parent_element
      RETURNING VALUE(ro_parent_element) TYPE REF TO zdab_if_uml_element_api,

    add_child_element
      IMPORTING io_child_element TYPE REF TO zdab_if_uml_element_api,
    get_child_elements
      RETURNING VALUE(rt_child_elements) TYPE zdab_t_uml_element,
    get_child_uml_code
      RETURNING VALUE(rv_child_uml_code) TYPE string,

    get_uml_code
      RETURNING VALUE(rv_uml_code) TYPE string.

ENDINTERFACE.
