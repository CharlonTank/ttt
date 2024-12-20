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
                    userPreference: (() => {
                        const stored = localStorage.getItem('darkMode');
                        if (stored === 'dark') return 'dark';
                        if (stored === 'light') return 'light';
                        if (stored === 'system-dark' || stored === 'system-light') return 'system';
                        return 'system';
                    })(),
                    systemMode: window.matchMedia('(prefers-color-scheme: dark)').matches ? 'dark' : 'light'
                }
            });
        } catch (e) {
            console.error('Error reading data:', e);
        }
    });

    window.matchMedia('(prefers-color-scheme: dark)').addEventListener('change', (e) => {
        app.ports.receiveLocalStorage_.send({
            localStorage: {
                language: localStorage.getItem('language') || ((navigator.language || '').toLowerCase().includes('fr') ? 'fr' : 'en'),
                userPreference: localStorage.getItem('darkMode') || 'system',
                systemMode: e.matches ? 'dark' : 'light'
            }
        });
    });
};
