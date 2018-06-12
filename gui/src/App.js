import React, { Component } from "react";
import { BrowserRouter as Router, Route } from "react-router-dom";
import { Button, Paper } from "@material-ui/core";
import InputIcon from "@material-ui/icons/Input";

import queryString from "./query-string";
import GraphInputDialog from "./GraphInputDialog";
import { Graph, EMPTYGRAPH } from "./Graph";

import "./App.css";
import PropTypes from "prop-types";

const paperstyle = {
  width: "100%",
  height: "100%",
  margin: 0,
  minHeight: "100%"
};
const butstyle = {
  right: 20,
  top: 20,
  position: "fixed"
};

class Main extends Component {
  state = {
    open: false,
    graph: EMPTYGRAPH
  };
  graphviz;
  handleClickOpen = () => {
    this.setState({ open: true });
  };
  dialogdone = (update, graph) => {
    this.setState({ open: false });
    if (update) {
      if (graph === EMPTYGRAPH) this.graphviz = null;
      this.setState({ graph });
    }
  };

  componentDidMount() {
    var graph = this.state.graph;
    if (this.props.location && this.props.location.search) {
      let params = queryString.parse(this.props.location.search);
      if (params.useparentsource === "true") {
        graph = "";
        let receiveMessage = event => {
          if (event.data.substr(0, 12) === "graphSource=") {
            this.setState({ graph: event.data.substr(12) });
          }
        };
        window.addEventListener("message", receiveMessage, false);
        window.parent.postMessage("requestGraphSource", "*");
      } else {
        graph = decodeURI(params.graphsource);
      }
    }
    if (graph && graph !== this.state.graph) {
      this.setState({ graph });
    }
  }
  render() {
    return (
      <Paper style={paperstyle}>
        <Graph graph={this.state.graph} />

        <Button
          variant="fab"
          aria-label="add"
          onClick={this.handleClickOpen}
          style={butstyle}
          className={PropTypes.object.isRequired.button}
        >
          <InputIcon />
        </Button>

        <GraphInputDialog
          openstate={this.state.open}
          done={this.dialogdone}
          graphsource={this.state.graph}
        />
      </Paper>
    );
  }
}

function App() {
  return (
    <Router>
      <Route path="/" component={Main} />
    </Router>
  );
}

export default App;
