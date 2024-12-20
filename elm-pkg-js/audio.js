exports.init = async function (app) {
    // Initialize audio context immediately
    const audioContext = new (window.AudioContext || window.webkitAudioContext)();
    const audioBuffers = {};
    const volumes = {
        'win': 0.5,
        'small-win': 0.5,
        'error': 0.5,
        'big-win': 0.5,
        'lose': 0.5,
        'default': 0.5,
        'button-click': 0.2,
        'move-x': 0.2,
        'move-o': 0.2,
        'draw': 0.2,
        'play-online': 0.2,
        'sound-1': 0.2,
        'sound-2': 0.2,
        'sound-3': 0.2,
        'sound': 0.2
    };

    // Load sounds immediately
    await loadSounds();

    // Load and decode audio files
    async function loadSounds() {
        const soundFiles = {
            'button-click': '/sounds/button-click.wav',
            'win': '/sounds/win.wav',
            'draw': '/sounds/draw.mp3',
            'move-x': '/sounds/move-x.mp3',
            'move-o': '/sounds/move-o.mp3',
            'error': '/sounds/error.mp3',
            'small-win': '/sounds/small-win.mp3',
            'play-online': '/sounds/play-online.mp3',
            'big-win': '/sounds/big-win.mp3',
            'lose': '/sounds/lose.wav',
            'sound-1': '/sounds/1.wav',
            'sound-2': '/sounds/2.wav',
            'sound-3': '/sounds/3.wav',
            'sound': '/sounds/sound.wav'
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
    }

    // Play a sound with the given name
    const playSound = (name) => {
        if (audioContext.state === 'suspended') {
            audioContext.resume();
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

    // Resume audio context on any user interaction
    document.addEventListener('click', () => {
        if (audioContext.state === 'suspended') {
            audioContext.resume();
        }
    });

    // Subscribe to port
    app.ports.playSound_.subscribe(function(soundName) {
        console.log("Playing sound:", soundName);
        playSound(soundName);
    });
}; 