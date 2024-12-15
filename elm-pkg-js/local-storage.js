exports.init = async function (app) {
    app.ports.storeLocalStorage_.subscribe(function (data) {
        try {
            const { key, value } = JSON.parse(data);
            localStorage.setItem(key, value);
        } catch (e) {
            console.error('Error storing data in localStorage:', e);
        }
    });

    app.ports.getLocalStorageValue_.subscribe(function (key) {
        try {
            const value = localStorage.getItem(key);
            app.ports.receiveLocalStorageValue_.send(JSON.stringify({
                key: key,
                value: value || ''
            }));
        } catch (e) {
            console.error('Error reading data from localStorage:', e);
        }
    });

    // Initial load of localStorage values
    try {
        const language = localStorage.getItem('language') || 'FR';
        const darkMode = localStorage.getItem('darkMode') === 'true';
        app.ports.receiveLocalStorage_.send(JSON.stringify({
            language: language,
            darkMode: darkMode
        }));
    } catch (e) {
        console.error('Error loading initial data from localStorage:', e);
        app.ports.receiveLocalStorage_.send(JSON.stringify({
            language: 'FR',
            darkMode: false
        }));
    }
}; 