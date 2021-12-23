const uuidV5 = require('uuid/v5')

exports.genStableUUID = () => {
  const stack = new Error().stack
  const stackLines = stack.split('\n')
  const hookCallSite = stackLines[stackLines.length - 3] // TODO: Add tests to ensure this stays correct
  return uuidV5(hookCallSite, '31877c6f-998d-44e6-99e6-3cd31a643f1d')
}
