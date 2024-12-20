const sounds = {
    'button-click': new Audio('/sounds/button-click.mp3')
};

// Initialize all sounds
Object.values(sounds).forEach(sound => {
    sound.load();
});

export function init(app) {
    app.ports.playSound.subscribe(function(soundName) {
        const sound = sounds[soundName];
        if (sound) {
            // Reset the audio to the start if it's already playing
            sound.currentTime = 0;
            sound.play().catch(function(error) {
                console.log("Error playing sound:", error);
            });
        }
    });
} 