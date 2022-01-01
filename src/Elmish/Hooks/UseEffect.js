const React = require("react")

class LifeCycles extends React.Component {
  constructor(props) {
    super(props)
  }

  componentDidUpdate(prevProps) {
    this.props.componentDidUpdate(prevProps.deps)
  }

  render() {
    return this.props.children
  }
}

exports.lifeCycles_ = LifeCycles
