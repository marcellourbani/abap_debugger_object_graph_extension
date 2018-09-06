*"* use this source file for any type of declarations (class
*"* definitions, interfaces or type declarations) you need for
*"* components in the private section
class bd definition inheriting from CL_TPDA_SCRIPT_DATA_DESCR.
  public section.
    class-methods:vname importing descr type ref to CL_TPDA_SCRIPT_DATA_DESCR
                  returning value(varname) type string.
endclass.
