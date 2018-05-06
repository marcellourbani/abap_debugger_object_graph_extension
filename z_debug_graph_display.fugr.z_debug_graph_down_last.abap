FUNCTION Z_DEBUG_GRAPH_DOWN_LAST.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     REFERENCE(FILE) TYPE  STRING DEFAULT '/tmp/graph.html'
*"----------------------------------------------------------------------
  data:source type string,
       itab   type table of string.

  import source from database indx(dg) id 'LASTGRAPH'.
  append source to itab.

  call function 'GUI_DOWNLOAD'
    exporting
      filename                = file
    tables
      data_tab                = itab
    exceptions
      file_write_error        = 1
      no_batch                = 2
      gui_refuse_filetransfer = 3
      invalid_type            = 4
      no_authority            = 5
      unknown_error           = 6
      header_not_allowed      = 7
      separator_not_allowed   = 8
      filesize_not_allowed    = 9
      header_too_long         = 10
      dp_error_create         = 11
      dp_error_send           = 12
      dp_error_write          = 13
      unknown_dp_error        = 14
      access_denied           = 15
      dp_out_of_memory        = 16
      disk_full               = 17
      dp_timeout              = 18
      file_not_found          = 19
      dataprovider_exception  = 20
      control_flush_error     = 21
      others                  = 22.
  if sy-subrc <> 0.
*   message id sy-msgid type sy-msgty number sy-msgno
*              with sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  endif.

endfunction.
