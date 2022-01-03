exports.newRef_ = x => ({ current: x })

exports.readRef_ = ref => ref.current

exports.writeRef_ = (ref, a) => { ref.current = a }
