const local_storage = require('./elm-pkg-js/local-storage')
const audio = require('./elm-pkg-js/audio')

exports.init = async function init(app) {
    local_storage.init(app)
    audio.init(app)
} 