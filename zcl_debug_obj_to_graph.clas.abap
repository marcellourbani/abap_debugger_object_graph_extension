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
class zcl_debug_obj_to_graph definition public  final  create public .

  public section.

    methods export_graph_to_clipboard
      importing
        !name type string .
    methods display_graph
      importing
        !name type string .
  protected section.
  private section.

    constants c_newline like cl_abap_char_utilities=>newline value cl_abap_char_utilities=>newline ##NO_TEXT.

    data:
      mt_visited    type table of string.
    data mv_graph type string.

    methods to_clipboard .
    methods name
      importing
        !iv_name       type string
      returning
        value(rv_name) type string .
    methods handle_dataref
      importing
        !iv_name  type string
        !io_descr type ref to cl_tpda_script_data_descr
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
        name         type string
      returning
        value(graph) type string
      raising
        cx_tpda .
    methods createhtmlwrapper
      importing
        baseurl         type string
      returning
        value(r_result) type string.
    methods get_temp_file_url
      returning
        value(r_result) type string.

endclass.



class zcl_debug_obj_to_graph implementation.


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


  method display_graph.
    data: viewer    type ref to cl_gui_html_viewer,
          container type ref to cl_gui_custom_container,
          url(400)  type c,
          lx_root   type ref to cx_root,
          contents  type string,
          itab      type table of string,
          filename  type string.
    try.
        if name is initial.
          return.
        endif.
        create_graph( name ).
        contents = createhtmlwrapper( baseurl = 'http://192.168.1.46:3000' ).
        append contents to itab.

        url = get_temp_file_url( ).
        if url <>  ''.
          filename = url.
          call function 'GUI_DOWNLOAD'
            exporting
              filename = filename
            tables
              data_tab = itab
            exceptions
              others   = 22.
        endif.
        if sy-subrc <> 0 or url = ''.
          message 'Error writing graph file' type 'I'.
          return.
        endif.

        create object viewer exporting parent = container.
        viewer->detach_url_in_browser(  url ).

      catch cx_tpda_varname.
        message 'Unknown variable'(006) type 'I'.
      catch cx_root into lx_root.
        message lx_root type 'I'.
    endtry.

    call function 'GUI_DOWNLOAD'
      exporting
        filename = '/tmp/visited'
      tables
        data_tab = mt_visited
      exceptions
        others   = 22.

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
          lo_descr type ref to cl_tpda_script_data_descr,
          lx_tpda  type ref to cx_tpda.

    try.
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
            handle_dataref( iv_name  = iv_name
                            io_descr = lo_descr ).
          when cl_tpda_script_data_descr=>mt_object.
            handle_object( iv_name  = iv_name
                           io_descr = lo_descr ).
          when cl_tpda_script_data_descr=>mt_objref.
            handle_objref( iv_name ).
        endcase.

      catch cx_tpda into lx_tpda.
    endtry.

  endmethod.


  method handle_dataref.
    data: ls_info    type tpda_scr_quick_info.

    field-symbols: <ls_symobjref> type tpda_sys_symbdatref.

    ls_info = cl_tpda_script_data_descr=>get_quick_info( iv_name ).
    assign ls_info-quickdata->* to <ls_symobjref>.

    if <ls_symobjref>-instancename <> '{O:initial}'.
      handle( <ls_symobjref>-instancename ).
    endif.

    mv_graph = |{ mv_graph }"{ name( iv_name ) }" [{ c_newline
      }label = "ref"{ c_newline
      }shape = "record" ];{ c_newline
      }"{ name( iv_name ) }" -> "{ name( <ls_symobjref>-instancename ) }";{ c_newline }|.


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
      }"{ name( iv_name ) }" -> "{ name( <ls_symobjref>-instancename ) }";{ c_newline }|.


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


    data: lv_label        type string,
          lv_edges        type string,
          lv_name         type string,
          lt_components   type tpda_script_struc_componentsit,
          lo_struct       type ref to cl_tpda_script_structdescr,
          lv_match_offset type i.

    field-symbols: <ls_component> like line of lt_components.

    lo_struct ?= io_descr.

    lo_struct->components( importing p_components_it = lt_components ).
    find regex '\*$' in iv_name match offset lv_match_offset.

    lv_label = 'Structure'(004).
    loop at lt_components assigning <ls_component>.
      lv_label = |{ lv_label } \|<f{ sy-tabix }> {
        name( <ls_component>-compname ) }\\{ c_newline }|.
      if lv_match_offset = 0.
        lv_name = |{ iv_name }-{ <ls_component>-compname }|.
      else.
        lv_name = |{ iv_name(lv_match_offset) }{ <ls_component>-compname }|.
      endif.
*      concatenate removestar( iv_name ) '-' <ls_component>-compname into lv_name.
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

  method createhtmlwrapper.
    data: lines      type table of string,
          graphlines type table of string,
          graph      type string,
          iframe     type string.
    field-symbols: <graphline> like line of graphlines.
    graph = mv_graph.
    replace all occurrences of '\' in graph with '\\'.
    replace all occurrences of '''' in graph with '\'''.

    split graph at cl_abap_char_utilities=>newline into table graphlines.

    concatenate  '<iframe src="' baseurl '?useparentsource=true">' into iframe.

    append:
  '<!DOCTYPE html>' to lines,
  '<html lang="en">' to lines,
  '' to lines,
  '<head>' to lines,
  '    <meta charset="utf-8">' to lines,
  '    <meta name="viewport" content="width=device-width, initial-scale=1, shrink-to-fit=no">' to lines,
  '    <title>Graph display</title>' to lines,
  '    <style>' to lines,
  '        iframe {' to lines,
  '            position: fixed;' to lines,
  '            top: 0px;' to lines,
  '            left: 0px;' to lines,
  '            bottom: 0px;' to lines,
  '            right: 0px;' to lines,
  '            width: 100%;' to lines,
  '            height: 100%;' to lines,
  '            border: none;' to lines,
  '            margin: 0;' to lines,
  '            padding: 0;' to lines,
  '            overflow: hidden;' to lines,
  '            z-index: 999999;' to lines,
  '        }' to lines,
  '    </style>' to lines,
  '    <script>' to lines,
  '        var graphsource =  ' to lines.
    loop at graphlines assigning <graphline>.

      concatenate '''' <graphline> '\n'' +' into graph.
      append graph to lines.

    endloop.

    append:
    '       '''' ;' to lines,
    '        function receiveMessage(event) {' to lines,
    '            if (event.data === "requestGraphSource") { ' to lines,
    '                event.source.postMessage("graphSource=" + graphsource, "*");' to lines,
    '            }' to lines,
    '        }' to lines,
    '        window.addEventListener("message", receiveMessage, false);' to lines,
    '    </script>' to lines,
    '</head>' to lines,
    '' to lines,
    '<body>' to lines,
    iframe to lines,
    '        Your browser doesn''t support iframes' to lines,
    '    </iframe>' to lines,
    '</body>' to lines,
    '' to lines,
    '</html>'  to lines.

    concatenate lines of lines into r_result separated by  cl_abap_char_utilities=>cr_lf.

  endmethod.


  method get_temp_file_url.
    data: separator type c,
          tempdir   type string,
          guid      type guid_32.

    cl_gui_frontend_services=>get_temp_directory(
      changing
        temp_dir             =  tempdir
      exceptions
        cntl_error           = 1
        error_no_gui         = 2
        not_supported_by_gui = 3
        others               = 4 ).
    if sy-subrc = 0.
      cl_gui_frontend_services=>get_file_separator(
        changing
          file_separator       = separator
        exceptions
          others               = 4 ).
    endif.

    if sy-subrc = 0.
      call function 'GUID_CREATE'
        importing
          ev_guid_32 = guid.

      r_result = |{ tempdir }{ separator }{ guid }.html|.
    endif.

  endmethod.

endclass.
