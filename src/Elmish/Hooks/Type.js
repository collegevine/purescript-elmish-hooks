const uuidV5 = require('uuid/v5')

exports.genStableUUID = () => uuidV5(new Error().stack, '31877c6f-998d-44e6-99e6-3cd31a643f1d')
