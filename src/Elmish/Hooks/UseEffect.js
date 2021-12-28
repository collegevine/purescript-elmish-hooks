const cache = {}

exports.generateComponentName = ({ name, value }) => {
  if (!cache[name]) {
    cache[name] = { key: 0, value: null }
  }

  if (cache[name].value !== value) {
    cache[name].value = value
    cache[name].key += 1
  }

  return `${name}-${cache[name].key}`
}
