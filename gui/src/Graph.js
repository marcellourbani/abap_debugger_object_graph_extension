import React, { Component } from "react";
import * as d3g from "d3-graphviz";
import Typography from "@material-ui/core/Typography";
import { MessageSnackbar } from "./MessageSnackbar";

export const EMPTYGRAPH = "digraph {}";
export class Graph extends Component {
  state = {
    open: false,
    message: ""
  };
  renderGraph() {
    if (this.props.graph !== EMPTYGRAPH) {
      if (!this.graphviz) this.graphviz = d3g.graphviz("#myGraph");
      try {
        this.graphviz.renderDot(this.props.graph);
      } catch (exception) {
        this.setState({ open: true, message: "Invalid graph source" });
      }
    }
  }

  componentDidUpdate(prevProps, prevState, snapshot) {
    if (prevProps.graph !== this.props.graph) this.renderGraph();
  }
  componentDidMount() {
    this.renderGraph();
  }

  render() {
    return this.props.graph === EMPTYGRAPH ? (
      <Typography variant="display4" gutterBottom={true}>
        Please edit/paste a graph to render it
      </Typography>
    ) : (
      <div id="myGraph">
        <MessageSnackbar
          isopen={this.state.open}
          message={this.state.message}
          close={() => this.setState({ open: false })}
        />;
      </div>
    );
  }
}

export default Graph;
