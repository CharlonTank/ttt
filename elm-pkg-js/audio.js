exports.init = async function (app) {
    let audioContext;
    const audioBuffers = {};
    const volumes = {
        'win': 0.7,
        'small-win': 0.6,
        'error': 0.4,
        'default': 0.5
    };

    // Initialize audio context on first user interaction
    const initAudioContext = () => {
        if (!audioContext) {
            audioContext = new (window.AudioContext || window.webkitAudioContext)();
            return loadSounds();
        }
        return Promise.resolve();
    };

    // Load and decode audio files
    const loadSounds = async () => {
        const soundFiles = {
            'button-click': '/sounds/button-click.mp3',
            'win': '/sounds/win.mp3',
            'draw': '/sounds/draw.mp3',
            'move-x': '/sounds/move-x.mp3',
            'move-o': '/sounds/move-o.mp3',
            'error': '/sounds/error.mp3',
            'small-win': '/sounds/small-win.mp3',
            'play-online': '/sounds/play-online.mp3'
        };

        const loadSound = async (name, url) => {
            try {
                const response = await fetch(url);
                const arrayBuffer = await response.arrayBuffer();
                const audioBuffer = await audioContext.decodeAudioData(arrayBuffer);
                audioBuffers[name] = audioBuffer;
            } catch (error) {
                console.error(`Error loading sound ${name}:`, error);
            }
        };

        const loadPromises = Object.entries(soundFiles).map(([name, url]) => 
            loadSound(name, url)
        );

        await Promise.all(loadPromises);
    };

    // Play a sound with the given name
    const playSound = (name) => {
        if (!audioContext) {
            initAudioContext();
            return;
        }

        const buffer = audioBuffers[name];
        if (!buffer) {
            console.warn(`Sound ${name} not loaded`);
            return;
        }

        // Create audio nodes
        const source = audioContext.createBufferSource();
        const gainNode = audioContext.createGain();
        
        // Connect nodes
        source.buffer = buffer;
        source.connect(gainNode);
        gainNode.connect(audioContext.destination);

        // Set volume
        gainNode.gain.value = volumes[name] || volumes.default;

        // Play the sound
        source.start(0);
    };

    // Resume audio context on user interaction
    document.addEventListener('click', () => {
        if (audioContext && audioContext.state === 'suspended') {
            audioContext.resume();
        }
    }, { once: true });

    // Subscribe to port
    app.ports.playSound_.subscribe(function(soundName) {
        console.log("Playing sound:", soundName);
        playSound(soundName);
    });
}; 