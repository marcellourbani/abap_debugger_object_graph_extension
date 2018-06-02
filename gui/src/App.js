import React, { Component } from "react";
import queryString from "query-string";
import * as d3g from "d3-graphviz";
import { MuiThemeProvider, createMuiTheme } from "material-ui/styles";
import { BrowserRouter as Router, Route } from "react-router-dom";

import { Button, Paper } from "material-ui";
import InputIcon from "@material-ui/icons/Input";
import "./App.css";
import PropTypes from "prop-types";
import GraphInputDialog from "./GraphInputDialog";

const theme = createMuiTheme();
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
    graph: "digraph {}"
  };
  graphviz;
  handleClickOpen = () => {
    this.setState({ open: true });
  };
  renderGraph = () => {
    if (!this.graphviz) this.graphviz = d3g.graphviz("#myGraph");
    this.graphviz.renderDot(this.state.graph);
  };
  dialogdone = (update, graph) => {
    this.setState({ open: false });
    if (update) {
      this.setState({ graph });
    }
  };
  componentDidUpdate(prevProps, prevState) {
    if (!prevState || prevState.graph !== this.state.graph) this.renderGraph();
  }

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
    } else if (graph) {
      this.renderGraph();
    }
  }
  render() {
    return (
      <Paper style={paperstyle}>
        <div id="myGraph" />

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
    <MuiThemeProvider theme={theme}>
      <Router>
        <Route path="/" component={Main} />
      </Router>
    </MuiThemeProvider>
  );
}

export default App;
