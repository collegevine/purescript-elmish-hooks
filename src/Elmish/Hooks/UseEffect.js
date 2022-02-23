import React from "react";

class UseEffectLifeCycles extends React.Component {
  componentDidUpdate(prevProps) {
    this.props.componentDidUpdate(prevProps.deps)
  }

  render() {
    return this.props.children
  }
}

export {UseEffectLifeCycles as useEffectLifeCycles_};
