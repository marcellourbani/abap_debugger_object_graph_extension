import React, { Component } from "react";
import { HashRouter as Router, Route, Link } from "react-router-dom";
import { Button } from "@material-ui/core";
import InputIcon from "@material-ui/icons/Input";

import queryString from "./query-string";
import GraphInputDialog from "./GraphInputDialog";
import { Graph, EMPTYGRAPH } from "./Graph";

import "./App.css";
import PropTypes from "prop-types";

const butstyle = {
  right: 20,
  top: 20,
  position: "fixed"
};

class MainGraph extends Component {
  state = {
    open: false,
    graphversion: 0,
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
      this.setState({ graph, graphversion: this.state.graphversion + 1 });
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
      <div className="full">
        <Graph
          graph={this.state.graph}
          graphversion={this.state.graphversion}
        />

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
      </div>
    );
  }
}
function Main() {
  return (
    <div>
      <h1>Graphviz browser</h1>
      This is a simple <a href="http://www.graphviz.org/">graphviz</a> browser
      developed for&nbsp;
      <a href="https://github.com/marcellourbani/abap_debugger_object_graph_extension">
        Abap debugger object graph extension
      </a>
      , which includes the sources for this page.
      <br />
      <br />
      <Link to="/graph">Click here for the actual graph browser</Link>
    </div>
  );
}

function App() {
  return (
    <Router>
      <div className="full">
        <Route path="/" component={Main} exact={true} />
        <Route path="/graph" component={MainGraph} exact={true} />
      </div>
    </Router>
  );
}

export default App;
