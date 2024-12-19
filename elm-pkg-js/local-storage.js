exports.init = async function (app) {
    app.ports.storeLocalStorageValue_.subscribe(data => {
        try {
            localStorage.setItem(data.key, data.value);
        } catch (e) {
            console.error('Error storing data:', e);
        }
    });

    app.ports.getLocalStorage_.subscribe(() => {
        try {
            app.ports.receiveLocalStorage_.send({
                localStorage: {
                    language: (() => {
                        switch (localStorage.getItem('language')) {
                            case 'fr':
                                return 'fr';
                            case 'en':
                                return 'en';
                            default:
                                return (navigator.language || '').toLowerCase().includes('fr') ? 'fr' : 'en';
                        }
                    })(),
                    darkMode: (() => {
                        switch (localStorage.getItem('darkMode')) {
                            case 'true':
                                return true;
                            case 'false':
                                return false;
                            default:
                                return window.matchMedia('(prefers-color-scheme: dark)').matches;
                        }
                    })()
                }
            });
        } catch (e) {
            console.error('Error reading data:', e);
        }
    });
};
