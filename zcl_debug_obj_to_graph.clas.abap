********************************************************************************
* The MIT License (MIT)
*
* Copyright (c) 2018 Marcello Urbani
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

    methods display_graph importing !name type string.
    methods popup returning value(varname) type string.
  protected section.
  private section.

    constants c_newline like cl_abap_char_utilities=>newline value cl_abap_char_utilities=>newline ##NO_TEXT.

    data uploadpath type string.
    data downloadpath type string.


    methods get_temp_file_url
      returning
        value(r_result) type string.


endclass.



class zcl_debug_obj_to_graph implementation.

  method display_graph.
    data: lx_root   type ref to cx_root,
          factory   type ref to zcl_debug_obj_graph_factory,
          graph     type ref to zcl_abap_graph.
    try.
        if name is initial.
          return.
        endif.
        factory = zcl_debug_obj_graph_factory=>create( ).
        graph   = factory->create_graph( name ).
        if graph is bound.
          zcl_abap_graph_utilities=>show_in_browser( graph = graph comments = factory->logs ).
        else.
          message 'Unknown variable'(006) type 'I'.
        endif.

      catch cx_tpda_varname.
        message 'Unknown variable'(006) type 'I'.
      catch cx_root into lx_root.
        message lx_root type 'I'.
    endtry.

  endmethod.

  method get_temp_file_url.
    data: separator type c,
          guid      type guid_32.

    cl_gui_frontend_services=>get_file_separator(
      changing
        file_separator       = separator
      exceptions
        others               = 4 ).
    if sy-subrc = 0.
      "will not work with local variables...
      cl_gui_frontend_services=>get_upload_download_path(
        changing
          upload_path                 =  uploadpath
          download_path               =  downloadpath
        exceptions
          others                      = 6 ).
    endif.
    if sy-subrc = 0.
      call function 'GUID_CREATE'
        importing
          ev_guid_32 = guid.

      if downloadpath is initial.

        concatenate guid '.html' into r_result.

      else.

        concatenate downloadpath separator guid '.html' into r_result.

      endif.
    endif.

  endmethod.

  method popup.
    data: ok type abap_bool.

    call function 'POPUP_GET_STRING'
      exporting
        label = 'Variable name'
      importing
        value = varname
        okay  = ok.

    if ok = abap_false.
      clear varname.
    else.
      translate varname to upper case.
    endif.

  endmethod.

endclass.
