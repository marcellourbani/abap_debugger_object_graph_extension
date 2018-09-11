# Abap debugger object graph extension

ABAP debugger extension to visualize objects as graphs via [graphviz](http://www.graphviz.org/)

### Installation

The prerequisite [ABAP Graph](https://github.com/marcellourbani/abapgraph) needs to be installed first, then install via [abapGit](https://github.com/larshp/abapGit)

Tested on netweaver 7.5, not sure about older releases

### Use

1.  Set breakpoint in source code
2.  In the new debugger go to "Objects" tab
3.  open the tools menu from the icon on the left ![image](https://user-images.githubusercontent.com/2453277/44873462-39cffc00-ac90-11e8-8815-45b797085919.png)

4.  select **View object as graph**

The object graph will be shown in a new browser window (Example from http://zevolving.com/2012/01/iterator-design-pattern-to-access-linked-list/ )

![image](https://user-images.githubusercontent.com/2453277/45267892-94c1da00-b46c-11e8-8759-411cb635c4d2.png)

Another example from the excellent [Writing Testable Code for ABAP](https://open.sap.com/courses/wtc1) opensap course
![image](https://user-images.githubusercontent.com/2453277/45267904-ed917280-b46c-11e8-8c6c-d57bd72083fa.png)

Originally inspired by
[ABAP-Object-Visualizer](https://github.com/larshp/ABAP-Object-Visualizer)

Uses [Graphviz-browser](https://github.com/marcellourbani/Graphviz-browser) to render the graph in a browser window
