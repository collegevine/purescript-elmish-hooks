const uuidV5 = require('uuid/v5')

const uniqueNameFromCurrentCallStack_ = ({ trace }) => ({ skipFrames }) => {
  const stack = new Error().stack
  const stackLines = stack.split('\n')
  // TODO: Add tests to ensure this stays correct: See
  // https://github.com/collegevine/purescript-elmish-hooks/pull/3#discussion_r774324284
  // for an example.
  const hookCallSite = stackLines[skipFrames + 1]
  if (trace) {
    console.log('Hook Call Site:')
    console.log(hookCallSite)
    console.log('Full Stack Trace:')
    console.log(stack)
  }
  return uuidV5(hookCallSite, '31877c6f-998d-44e6-99e6-3cd31a643f1d')
}

exports.uniqueNameFromCurrentCallStack_ = uniqueNameFromCurrentCallStack_({ trace: false })
exports.uniqueNameFromCurrentCallStackTraced_ = uniqueNameFromCurrentCallStack_({ trace: true })
