const stackTraceParser = require('stacktrace-parser')

const uniqueNameFromCurrentCallStack_ = ({ trace }) => ({ skipFrames, prefix }) => {
  const stack = new Error().stack
  const stackLines = stackTraceParser.parse(stack)
  const hookCallSite = stackLines[skipFrames]
  const file = cleanName(hookCallSite.file.replace(/^(http(s?):\/\/)?[^\/]+/, ''))
  const methodName = cleanName(hookCallSite.methodName)
  if (trace) {
    console.log('Hook Call Site:')
    console.log(hookCallSite)
    console.log('Full Stack Trace:')
    console.log(stack)
  }
  return `${prefix}_${file}_${methodName}_${hookCallSite.lineNumber}_${hookCallSite.column}`
}

const cleanName = name => name.replace(/[^\d\w]+/g, '_').replace(/(^_|_$)/g, '')

exports.uniqueNameFromCurrentCallStack_ = uniqueNameFromCurrentCallStack_({ trace: false })
exports.uniqueNameFromCurrentCallStackTraced_ = uniqueNameFromCurrentCallStack_({ trace: true })
