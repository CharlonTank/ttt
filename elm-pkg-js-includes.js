const local_storage = require('./elm-pkg-js/local-storage')

exports.init = async function init(app) {
    local_storage.init(app)
} 