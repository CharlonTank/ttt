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
                        const stored = localStorage.getItem('darkMode');
                        if (stored === 'dark') return 'dark';
                        if (stored === 'light') return 'light';
                        if (stored === 'system-dark') return 'system-dark';
                        if (stored === 'system-light') return 'system-light';
                        // If not set, determine system mode:
                        return window.matchMedia('(prefers-color-scheme: dark)').matches
                            ? 'system-dark'
                            : 'system-light';
                    })()
                }
            });
        } catch (e) {
            console.error('Error reading data:', e);
        }
    });
};
