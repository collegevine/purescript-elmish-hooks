const React = require("react")

class UseEffectLifeCycles extends React.Component {
  componentDidUpdate(prevProps) {
    this.props.componentDidUpdate(prevProps.deps)
  }

  render() {
    return this.props.children
  }
}

exports.useEffectLifeCycles_ = UseEffectLifeCycles
