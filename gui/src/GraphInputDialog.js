import React from "react";
import { Button, TextField } from "@material-ui/core";
import {
  Dialog,
  DialogActions,
  DialogContent,
  DialogContentText,
  DialogTitle
} from "@material-ui/core";

const GraphInputDialog = ({ openstate, done, graphsource }) => {
  var source = graphsource;
  const handleClose = () => {
    done(false);
  };
  const handleRender = () => {
    done(true, source);
  };
  const handleGraphChange = event => {
    source = event.target.value;
  };
  return (
    <Dialog open={openstate} fullWidth={true}>
      <DialogTitle id="form-dialog-title">Enter Graph</DialogTitle>
      <DialogContent>
        <DialogContentText>
          Edit or paste the graph source below
        </DialogContentText>
        <TextField
          autoFocus
          margin="dense"
          id="name"
          label="Graph source"
          multiline={true}
          rows="20"
          fullWidth
          onChange={handleGraphChange}
          defaultValue={source}
        />
      </DialogContent>
      <DialogActions>
        <Button onClick={handleClose} color="primary">
          Cancel
        </Button>
        <Button onClick={handleRender} color="primary">
          Render
        </Button>
      </DialogActions>
    </Dialog>
  );
};

export default GraphInputDialog;
