import * as stackTraceParser from 'stacktrace-parser'

export function uniqueNameFromCurrentCallStack_({ skipFrames, prefix }) {
  const stack = new Error().stack
  const stackLines = stackTraceParser.parse(stack)
  const hookCallSite = stackLines[skipFrames]
  const file = cleanName(hookCallSite.file.replace(/^(http(s?):\/\/)?[^\/]+/, ''))
  const methodName = cleanName(hookCallSite.methodName)
  return `${prefix}_${file}_${methodName}_${hookCallSite.lineNumber}_${hookCallSite.column}`
}

const cleanName = name => name.replace(/[^\d\w]+/g, '_').replace(/(^_|_$)/g, '')
