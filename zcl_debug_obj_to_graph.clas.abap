********************************************************************************
* The MIT License (MIT)
*
* Copyright (c) 2018 Marcello Urbani
* Copyright (c) 2015 Lars Hvam Petersen
*
* Permission is hereby granted, free of charge, to any person obtaining a copy
* of this software and associated documentation files (the "Software"), to deal
* in the Software without restriction, including without limitation the rights
* to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
* copies of the Software, and to permit persons to whom the Software is
* furnished to do so, subject to the following conditions:
*
* The above copyright notice and this permission notice shall be included in all
* copies or substantial portions of the Software.
*
* THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
* IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
* FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
* AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
* LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
* OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
* SOFTWARE.
********************************************************************************
class zcl_debug_obj_to_graph definition
  public
  final
  create public .

  public section.

    methods export_graph_to_clipboard
      importing
        !name type string .
  protected section.
  private section.

    constants c_newline like cl_abap_char_utilities=>newline value cl_abap_char_utilities=>newline ##NO_TEXT.
    data:
      mt_visited type table of string .
    data mv_graph type string .

    methods to_clipboard .
    methods name
      importing
        !iv_name       type string
      returning
        value(rv_name) type string .
    methods handle_dataref
      importing
        !iv_name type string
      raising
        cx_tpda .
    methods handle_tab
      importing
        !iv_name  type string
        !io_descr type ref to cl_tpda_script_data_descr
      raising
        cx_tpda .
    methods handle_object
      importing
        !iv_name  type string
        !io_descr type ref to cl_tpda_script_data_descr
      raising
        cx_tpda .
    methods handle_objref
      importing
        !iv_name type string
      raising
        cx_tpda .
    methods handle_string
      importing
        !iv_name  type string
        !io_descr type ref to cl_tpda_script_data_descr
      raising
        cx_tpda .
    methods handle_simple
      importing
        !iv_name  type string
        !io_descr type ref to cl_tpda_script_data_descr
      raising
        cx_tpda .
    methods handle_struct
      importing
        !iv_name  type string
        !io_descr type ref to cl_tpda_script_data_descr
      raising
        cx_tpda .
    methods handle
      importing
        !iv_name type string
      raising
        cx_tpda .
    methods create_graph
      importing
        !name        type string
      returning
        value(graph) type string
      raising
        cx_tpda .
ENDCLASS.



CLASS ZCL_DEBUG_OBJ_TO_GRAPH IMPLEMENTATION.


  method create_graph.
    clear: mt_visited,mv_graph.

    handle( name ) .

    mv_graph = |digraph g \{{ c_newline
      }graph [{ c_newline
      }rankdir = "LR"{ c_newline
      }];{ c_newline
      }{ mv_graph }{ c_newline
      }\}|.

    graph = mv_graph.
  endmethod.


  method export_graph_to_clipboard.
    data: lx_tpda type ref to cx_tpda.

    try.
        if name is initial.
          return.
        endif.
        create_graph( name ).
        to_clipboard( ).
      catch cx_tpda_varname.
        message 'Unknown variable'(006) type 'I'.
      catch cx_tpda into lx_tpda.
        message lx_tpda type 'I'.
    endtry.

  endmethod.


  method handle.


    data: ls_info  type tpda_scr_quick_info,
          lo_descr type ref to cl_tpda_script_data_descr.


    read table mt_visited from iv_name transporting no fields.
    if sy-subrc = 0.
      return.
    endif.
    append iv_name to mt_visited.

    lo_descr = cl_tpda_script_data_descr=>factory( iv_name ).
    ls_info = cl_tpda_script_data_descr=>get_quick_info( iv_name ).

    case ls_info-metatype.
      when cl_tpda_script_data_descr=>mt_simple.
        handle_simple( iv_name  = iv_name
                       io_descr = lo_descr ).
      when cl_tpda_script_data_descr=>mt_struct.
        handle_struct( iv_name  = iv_name
                       io_descr = lo_descr ).
      when cl_tpda_script_data_descr=>mt_string.
        handle_string( iv_name  = iv_name
                       io_descr = lo_descr ).
      when cl_tpda_script_data_descr=>mt_tab.
        handle_tab( iv_name  = iv_name
                    io_descr = lo_descr ).
      when cl_tpda_script_data_descr=>mt_datref.
        handle_dataref( iv_name ).
      when cl_tpda_script_data_descr=>mt_object.
        handle_object( iv_name  = iv_name
                       io_descr = lo_descr ).
      when cl_tpda_script_data_descr=>mt_objref.
        handle_objref( iv_name ).
    endcase.


  endmethod.


  method handle_dataref.
   "TODO implement this
    break developer.
    raise exception type cx_tpda.

  endmethod.


  method handle_object.


    data: lo_object     type ref to cl_tpda_script_objectdescr,
          lv_name       type string,
          lv_label      type string,
          lv_color      type string,
          lv_edges      type string,
          lv_type       type string,
          lt_attributes type tpda_script_object_attribut_it.

    field-symbols: <ls_attribute> like line of lt_attributes.


    lo_object ?= io_descr.
    lt_attributes = lo_object->attributes( ).

    lv_label = 'Object'(002).
    loop at lt_attributes assigning <ls_attribute>.
      lv_label = |{ lv_label } \|<f{ sy-tabix }> {
        name( <ls_attribute>-name ) }\\{ c_newline }|.
      concatenate iv_name '-' <ls_attribute>-name into lv_name.

      case <ls_attribute>-acckind.
        when if_tpda_control=>ak_private.
          lv_color = 'red'.
          lv_type = 'private'.
        when if_tpda_control=>ak_protected.
          lv_color = 'yellow'.
          lv_type = 'protected'.
        when if_tpda_control=>ak_public.
          lv_color = 'green'.
          lv_type = 'public'.
        when others.
          assert 1 = 1 + 1.
      endcase.
      lv_edges = |{ lv_edges }"{ name( iv_name ) }":<f{ sy-tabix
        }> -> "{ name( lv_name ) }" [fontcolor={ lv_color
        } label="{ lv_type }"];{ c_newline }|.

      handle( lv_name ).
    endloop.

    mv_graph = |{ mv_graph }"{ name( iv_name ) }" [{ c_newline
      }label = "{ lv_label }"{ c_newline
      }shape = "record" ];{ c_newline
      }{ lv_edges }|.


  endmethod.


  method handle_objref.


    data: ls_info  type tpda_scr_quick_info,
          lv_label type string.

    field-symbols: <ls_symobjref> type tpda_sys_symbobjref.


    ls_info = cl_tpda_script_data_descr=>get_quick_info( iv_name ).

    assign ls_info-quickdata->* to <ls_symobjref>.
    if <ls_symobjref>-instancename <> '{O:initial}'.
      handle( <ls_symobjref>-instancename ).
    endif.

    if iv_name ca '{'.
      lv_label = 'ref'.
    else.
      lv_label = iv_name.
    endif.

    mv_graph = |{ mv_graph }"{ name( iv_name ) }" [{ c_newline
      }label = "{ lv_label }"{ c_newline
      }shape = "record" ];{ c_newline
      }"{ name( iv_name ) }" -> "{ name( <ls_symobjref>-instancename ) }";{ c_newline
      }|.


  endmethod.


  method handle_simple.


    data: lo_elem  type ref to cl_tpda_script_elemdescr,
          lv_value type string.


    lo_elem ?= io_descr.
    lv_value = lo_elem->value( ).

    replace all occurrences of c_newline in lv_value with space.

    mv_graph = |{ mv_graph }"{ name( iv_name ) }" [{ c_newline
      }label = "{ lv_value }"{ c_newline
      }shape = "record" ];{ c_newline }|.


  endmethod.


  method handle_string.


    data: lo_string type ref to cl_tpda_script_stringdescr,
          lv_value  type string.


    lo_string ?= io_descr.
    lv_value = lo_string->value( ).

    replace all occurrences of c_newline in lv_value with space.

    mv_graph = |{ mv_graph }"{ name( iv_name ) }" [{ c_newline
      }label = "'{ lv_value }'"{ c_newline
      }shape = "record" ];{ c_newline }|.


  endmethod.


  method handle_struct.


    data: lv_label      type string,
          lv_edges      type string,
          lv_name       type string,
          lt_components type tpda_script_struc_componentsit,
          lo_struct     type ref to cl_tpda_script_structdescr.

    field-symbols: <ls_component> like line of lt_components.


    lo_struct ?= io_descr.

    lo_struct->components( importing p_components_it = lt_components ).

    lv_label = 'Structure'(004).
    loop at lt_components assigning <ls_component>.
      lv_label = |{ lv_label } \|<f{ sy-tabix }> {
        name( <ls_component>-compname ) }\\{ c_newline }|.
      concatenate iv_name '-' <ls_component>-compname into lv_name.
      lv_edges = |{ lv_edges }"{ name( iv_name ) }":<f{ sy-tabix
        }> -> "{ name( lv_name ) }";{ c_newline }|.

      handle( lv_name ).
    endloop.

    mv_graph = |{ mv_graph }"{ name( iv_name ) }" [{ c_newline
      }label = "{ lv_label }"{ c_newline
      }shape = "record" ];{ c_newline
      }{ lv_edges }|.


  endmethod.


  method handle_tab.


    data: lv_name  type string,
          lv_label type string,
          lv_edges type string,
          lo_table type ref to cl_tpda_script_tabledescr.


    lo_table ?= io_descr.
    lv_label = 'Table'(001).
    do lo_table->linecnt( ) times.
      lv_name = |{ iv_name }[{ sy-index }]|.

      lv_label = |{ lv_label } \|<f{ sy-index }> { sy-index }\\{ c_newline }|.
      lv_edges = |{ lv_edges }"{ name( iv_name ) }":<f{ sy-index
        }> -> "{ name( lv_name ) }";{ c_newline }|.

      handle( lv_name ).
    enddo.

    mv_graph = |{ mv_graph }"{ name( iv_name ) }" [{ c_newline
      }label = "{ lv_label }"{ c_newline
      }shape = "record" ];{ c_newline
      }{ lv_edges }|.


  endmethod.


  method name.


    rv_name = iv_name.
    replace all occurrences of '{' in rv_name with '\{'.
    replace all occurrences of '}' in rv_name with '\}'.


  endmethod.


  method to_clipboard.


    data: lt_table type standard table of abaptxt255 ##NEEDED,
          lv_rc    type i.


    split mv_graph at c_newline into table lt_table.

    cl_gui_frontend_services=>clipboard_export(
      importing
        data                 = lt_table
      changing
        rc                   = lv_rc
      exceptions
        cntl_error           = 1
        error_no_gui         = 2
        not_supported_by_gui = 3
        no_authority         = 4
        others               = 5 ).                       "#EC CI_SUBRC
    assert sy-subrc = 0.

    message 'Exported to clipboard'(003) type 'I'.


  endmethod.
ENDCLASS.
