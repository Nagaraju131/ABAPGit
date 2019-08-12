*&---------------------------------------------------------------------*
*& Report ZOOPS_INTERFACE_EX_01
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zoops_interface_ex_01.

CLASS lcl_rate_system DEFINITION.
  PUBLIC SECTION.
    TYPES:
      BEGIN OF ty_rating,
        name    TYPE char30,
        field1  TYPE i,
        field2  TYPE i,
        field3  TYPE i,
        field4  TYPE i,
        field5  TYPE i,
        field6  TYPE i,
        field7  TYPE i,
        field8  TYPE i,
        field9  TYPE i,
        field10 TYPE i,
        field11 TYPE i,
        field12 TYPE i,
        field13 TYPE i,
        field14 TYPE i,
      END OF ty_rating.
    TYPES ty_rating_tab TYPE STANDARD TABLE OF ty_rating WITH KEY name.

    METHODS get_impl.
    METHODS get_directors.

  PROTECTED SECTION.

  PRIVATE SECTION.
    DATA: r_rate    TYPE REF TO zif_rating_system.
    DATA it_rating  TYPE ty_rating_tab.
    DATA it_impl    TYPE seor_implementing_keys.
    DATA o_alv      TYPE REF TO cl_salv_table.
ENDCLASS.

CLASS lcl_rate_system IMPLEMENTATION.
*--------------------------------------------------------------------*
*
*--------------------------------------------------------------------*
  METHOD get_impl.
    DATA l_inf TYPE seoclskey VALUE 'ZIF_RATING_SYSTEM'.

    CALL FUNCTION 'SEO_INTERFACE_IMPLEM_GET_ALL'
      EXPORTING
        intkey       = l_inf
      IMPORTING
        impkeys      = it_impl
      EXCEPTIONS
        not_existing = 1
        OTHERS       = 2.
    IF sy-subrc <> 0.
    ENDIF.
  ENDMETHOD.
*--------------------------------------------------------------------*
*
*--------------------------------------------------------------------*
  METHOD get_directors.
    DATA lt_directors TYPE zif_rating_system=>ty_directors_tab.
    DATA ls_rating    TYPE ty_rating.
    DATA lv_name      TYPE char30.

    lt_directors = VALUE #(  ( name = CONV string( 'Steven Spielberg' ) )
                             ( name = CONV string( 'Martin Scorsese' ) )
                             ( name = CONV string( 'Quentin Tarantino' ) )
                             ( name = CONV string( 'Christopher Nolan' ) )
                             ( name = CONV string( 'James Cameron' ) )
                             ( name = CONV string( 'David Fincher' ) )
                             ( name = CONV string( 'Peter Jackson' ) )
                             ( name = CONV string( 'Michael Bay' ) ) ).
    LOOP AT it_impl INTO DATA(ls_impl).
      CREATE OBJECT r_rate TYPE (ls_impl).
      IF r_rate IS BOUND.
        r_rate->rating( CHANGING cv_name = lv_name ct_directors = lt_directors ).
      ENDIF.
      INSERT INITIAL LINE INTO it_rating.
      ls_rating-name = lv_name.
      LOOP AT lt_directors ASSIGNING FIELD-SYMBOL(<fs>).
        CASE <fs>-name.
          WHEN 'Steven Spielberg'.
            ls_rating-field1 = <fs>-rating.
          WHEN 'Martin Scorsese'.
            ls_rating-field2 = <fs>-rating.
          WHEN 'Quentin Tarantino'.
            ls_rating-field3 = <fs>-rating.
          WHEN 'Christopher Nolan'.
            ls_rating-field4 = <fs>-rating.
          WHEN 'James Cameron'.
            ls_rating-field5 = <fs>-rating.
          WHEN 'David Fincher'.
            ls_rating-field6 = <fs>-rating.
          WHEN 'Peter Jackson'.
            ls_rating-field7 = <fs>-rating.
          WHEN 'Michael Bay'.
            ls_rating-field8 = <fs>-rating.
        ENDCASE.
      ENDLOOP.
      APPEND ls_rating TO it_rating.
    ENDLOOP.
  ENDMETHOD.
ENDCLASS.

START-OF-SELECTION.