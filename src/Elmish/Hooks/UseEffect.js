const cache = {}

exports.get = (name) => cache[name]

exports.set = (name) => (value) => { cache[name] = value }
