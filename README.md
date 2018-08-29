# Abap debugger object graph extension

ABAP debugger extension to visualize objects as graphs via [graphviz](http://www.graphviz.org/)

### Installation

Install via [abapGit](https://github.com/larshp/abapGit)

Tested on netweaver 7.5, not sure about older releases

### Use

1.  Set breakpoint in source code
2.  In the new debugger go to "Objects" tab
3.  open the tools menu from the icon on the left
4.  select **View object as graph**
5.  Paste clipboard to http://www.webgraphviz.com/ and click "Generate Graph!"

I plan to replace the copy and paste with a BSP to display the graph when I find the time

Based on [ABAP-Object-Visualizer](https://github.com/larshp/ABAP-Object-Visualizer)
